{-# LANGUAGE LambdaCase #-}
{- | Gradually brighten & increase color temperature to ease waking up.

This starts at pure red & gets to 6500K and full brightness in 60 minutes,
or faster if you are already in the wakeup sequence.

TODO:
You can stretch or shrink this by specifying the @--time-multiplier@ flags.
E.g., a value of @1.5@ would give a total run-time of 90 minutes.

TODO:
You can also customize the transition scheme by passing YAML file to
@--transition-table@@.

TODO:
Finally, you can override the starting color & brightness with the
@--start-color@ and @--start-brightness@ flags.

TODO:
See @--help@ for additional information.


-}
import           Control.Concurrent             ( threadDelay )
import           Control.Monad                  ( when )
import           Data.Default                   ( Default(def) )
import           Data.Maybe                     ( mapMaybe )
import           Data.Time.Format               ( formatTime
                                                , defaultTimeLocale
                                                )
import           Data.Time.LocalTime            ( getZonedTime )
import           Network.Socket                 ( withSocketsDo )
import           System.Exit                    ( exitFailure )

import           HkHue.Config                   ( getConfig
                                                , ClientConfig(..)
                                                )
import           HkHue.Messages                 ( ClientMsg(..)
                                                , DaemonMsg(..)
                                                , LightData(..)
                                                , LightColor(..)
                                                , RGBColor(..)
                                                , LightPower(On)
                                                , StateUpdate(..)
                                                , sendMessage
                                                , receiveMessage
                                                )

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
    printWithDate "Starting wake up sequence."
    config <- getConfig
    withSocketsDo $ WS.runClient (configDaemonAddress config)
                                 (configDaemonPort config)
                                 "/"
                                 run
    printWithDate "Wake up sequence complete."


run :: WS.Connection -> IO ()
run conn = do
    sendMessage conn GetLightInfo
    lightData <- receiveMessage conn >>= \case
        Just (LightInfo i) -> return i
        _ -> putStrLn "Could not fetch light data." >> exitFailure
    let (ramp, startRed) = case currentStage lightData of
            Nothing    -> (temperatureRamp, True)
            Just stage -> (drop (stage + 1) temperatureRamp, False)
    when startRed $ do
        printWithDate "Turning on red lights at lowest brightness."
        sendMessage conn $ SetAllState def { suColor = Just $ RGBColor 255 0 0
                                           , suBrightness = Just 1
                                           , suPower = Just On
                                           }
        threadDelay 1000000
    mapM_ (setColorAndWait conn) ramp


setColorAndWait :: WS.Connection -> (Int, Int, Int) -> IO ()
setColorAndWait conn (colorTemp, brightness, minutes) = do
    sendMessage conn $ SetAllState def
        { suColorTemperature = Just colorTemp
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
