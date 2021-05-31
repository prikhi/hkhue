{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{- | Gradually brighten & increase color temperature to ease waking up.

This starts at pure red & gets to 6500K and full brightness in 60 minutes,
or faster if you are already in the wakeup sequence.

You can specify the groups to operate on using the @--group@ flag.
E.g., @--group Bedroom --group Bathroom@ would only affect the lights in @Bedroom@
and @Bathroom@ groups.

TODO:
You can stretch or shrink this by specifying the @--time-multiplier@ flags.
E.g., a value of @1.5@ would give a total run-time of 90 minutes.

TODO:
You can also customize the transition scheme by passing YAML file to
@--transition-table@@.

TODO:
Finally, you can override the starting color & brightness with the
@--start-color@ and @--start-brightness@ flags.

See @--help@ for additional information.


-}
import           Control.Concurrent             ( threadDelay )
import           Control.Monad                  ( when
                                                , forM_
                                                )
import           Data.Aeson                     ( FromJSON(..)
                                                , (.:?)
                                                , withObject
                                                , Value(..)
                                                )
import           Data.Data                      ( Data )
import           Data.Default                   ( Default(def) )
import           Data.List                      ( nub )
import           Data.Maybe                     ( mapMaybe
                                                , fromMaybe
                                                )
import           Control.Monad.Trans.Maybe      ( MaybeT(..)
                                                , runMaybeT
                                                )
import           Data.Time.Format               ( formatTime
                                                , defaultTimeLocale
                                                )
import           Data.Time.LocalTime            ( getZonedTime )
import           Data.Typeable                  ( Typeable )
import           Network.Socket                 ( withSocketsDo )
import           System.Console.CmdArgs         ( Annotate(..)
                                                , Ann
                                                , (+=)
                                                , cmdArgs_
                                                , help
                                                , helpArg
                                                , record
                                                , name
                                                , explicit
                                                , typ
                                                , program
                                                , summary
                                                )
import           System.Exit                    ( exitFailure )

import           HkHue.Config                   ( getConfig
                                                , ClientConfig(..)
                                                )
import           HkHue.Messages                 ( ClientMsg(..)
                                                , DaemonMsg(..)
                                                , LightData(..)
                                                , GroupIdentifier(..)
                                                , GroupData(..)
                                                , LightColor(..)
                                                , RGBColor(..)
                                                , LightPower(On)
                                                , StateUpdate(..)
                                                , sendMessage
                                                , receiveMessage
                                                )

import qualified Data.Text                     as T
import qualified Network.WebSockets            as WS

{-| The (Temperature, Brightness, Minutes) Ramp Up. -}
temperatureRamp :: [(Int, Int, Int)]
temperatureRamp =
    [ (2000, 10 , 5)
    , (2500, 15 , 5)
    , (3000, 30 , 7)
    , (3500, 60 , 7)
    , (4250, 80 , 10)
    , (5000, 100, 10)
    , (6500, 100, 15)
    ]


main :: IO ()
main = do
    rawArgs    <- cmdArgs_ arguments
    configArgs <- getConfig
    let args = if groups rawArgs == [] then configArgs else rawArgs
    printWithDate "Starting wake up sequence."
    config <- getConfig
    withSocketsDo $ WS.runClient (configDaemonAddress config)
                                 (configDaemonPort config)
                                 "/"
                                 (run args)
    printWithDate "Wake up sequence complete."


newtype Args
    = Args
        { groups :: [String]
        } deriving (Data, Typeable)

-- | Allow parsing Args from config file as well.
instance FromJSON Args where
    parseJSON = withObject "Args" $ \o -> fmap (fromMaybe def) $ runMaybeT $ do
        inner <- MaybeT $ o .:? "wakeup"
        gs    <- MaybeT $ withObject "Wakeup" (\v -> v .:? "groups") inner
        Args <$> mapM parseGroup gs
      where
        parseGroup :: MonadFail m => Value -> m String
        parseGroup = \case
            String s -> return $ T.unpack s
            Number n -> return $ show @Int $ floor n
            unexp ->
                fail
                    $  "Group ID/Name: Expected STRING or NUMBER, got "
                    <> show unexp

-- | Config file args defaults to all groups.
instance Default Args where
    def = Args []

-- | Parse CLI args
arguments :: Annotate Ann
arguments =
    record
            (Args [])
            [ groups
              := []
              += name "group"
              += name "g"
              += explicit
              += typ "GROUP"
              += help
                     "Group ID or name to operate on.\nMultiple flags operates on multiple groups."
            ]
        += help
               "Gradually increase color temperature & brightness of your lights."
        += helpArg [name "h"]
        += program "wakeup"
        += summary "hkhue wakeup script"


run :: Args -> WS.Connection -> IO ()
run args conn = do
    sendMessage conn GetLightInfo
    lightData <- receiveMessage conn >>= \case
        Just (LightInfo i) -> return i
        _ -> putStrLn "Could not fetch light data." >> exitFailure
    groupData <- if null $ groups args
        then return []
        else sendMessage conn GetGroupInfo >> receiveMessage conn >>= \case
            Just (GroupInfo i) -> return $ filter inGroupArgs i
            _ -> putStrLn "Could not fetch group data." >> exitFailure
    let groupLights = nub $ concatMap (map fst . gdLights) groupData
        lights      = if null groupData
            then lightData
            else filter ((`elem` groupLights) . ldId) lightData
        (ramp, startRed) = case currentStage lights of
            Nothing    -> (temperatureRamp, True)
            Just stage -> (drop (stage + 1) temperatureRamp, False)
    when startRed $ do
        printWithDate "Turning on red lights at lowest brightness."
        setState
            conn
            groupData
            def { suColor      = Just $ RGBColor 255 0 0
                , suBrightness = Just 1
                , suPower      = Just On
                }
        threadDelay 1000000
    mapM_ (setColorAndWait groupData conn) ramp
  where
    inGroupArgs :: GroupData -> Bool
    inGroupArgs gd =
        T.unpack (gdName gd)
            `elem` groups args
            ||     show (gdId gd)
            `elem` groups args



setColorAndWait :: [GroupData] -> WS.Connection -> (Int, Int, Int) -> IO ()
setColorAndWait groupData conn (colorTemp, brightness, minutes) = do
    setState
        conn
        groupData
        def { suColorTemperature = Just colorTemp
            , suBrightness       = Just brightness
            , suTransitionTime   = Just $ minutes * 60 * 10
            }
    threadDelay $ minutes * 60 * 1000000
    printWithDate
        $  "Reached "
        <> show colorTemp
        <> "K and "
        <> show brightness
        <> "% brightness in "
        <> show minutes
        <> " minute(s)."


printWithDate :: String -> IO ()
printWithDate str = do
    time <- getZonedTime
    let formattedTime = formatTime defaultTimeLocale "%T" time
    putStrLn $ "[" <> formattedTime <> "] " <> str


{- | Return the index of the current stage of the wakeup sequence.

Matching is done by the average color temperature, falling
back to the average brightness if all lights are in RGB mode instead of CT
mode.
-}
currentStage :: [LightData] -> Maybe Int
currentStage lightData =
    let lights             = filter ((== On) . ldPower) lightData
        averageTemperature = average $ mapMaybe
            ( (\case
                  RGBMode _  -> Nothing
                  CTMode  ct -> Just ct
              )
            . ldColor
            )
            lights
        averageBrightness = average $ map ldBrightness lights
    in  case averageTemperature of
            Just ct -> findStage (\(t, _, _) -> t) ct temperatureRamp 0
            Nothing -> case averageBrightness of
                Just bri -> findStage (\(_, b, _) -> b) bri temperatureRamp 0
                Nothing  -> Nothing
  where
    average xs = if null xs then Nothing else Just $ sum xs `div` length xs
    findStage selector target stages index = case stages of
        current : next : rest ->
            if selector current <= target && selector next > target
                then Just index
                else findStage selector target (next : rest) (index + 1)
        [final] -> if selector final <= target then Just index else Nothing
        []      -> Nothing

-- | Set the light state of the given groups or all lights if no groups
-- present.
setState :: WS.Connection -> [GroupData] -> StateUpdate -> IO ()
setState conn groupData stateUpdate = if null groupData
    then sendMessage conn $ SetAllState stateUpdate
    else forM_ groupData $ \gd ->
        sendMessage conn $ SetGroupState (GroupId $ gdId gd) stateUpdate
