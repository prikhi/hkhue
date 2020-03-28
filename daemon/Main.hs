{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
module Main
    ( main
    )
where

import           Control.Applicative            ( (<|>) )
import           Control.Concurrent             ( MVar
                                                , newMVar
                                                , readMVar
                                                , modifyMVar
                                                , modifyMVar_
                                                , threadDelay
                                                , forkIO
                                                , killThread
                                                )
import           Control.Exception.Safe         ( handleAny
                                                , finally
                                                )
import           Control.Monad                  ( (>=>)
                                                , unless
                                                , forever
                                                , forM_
                                                )
import           Control.Monad.Reader           ( ReaderT
                                                , runReaderT
                                                , ask
                                                , liftIO
                                                )
import           Data.Aeson                     ( (.:)
                                                , FromJSON(..)
                                                , encode
                                                , eitherDecode'
                                                , withObject
                                                )
import           Data.Default                   ( Default(..) )
import           Filesystem                     ( isFile
                                                , createTree
                                                )
import           Filesystem.Path.CurrentOS      ( decodeString )
import           System.Environment.XDG.BaseDir ( getUserDataFile
                                                , getUserDataDir
                                                )

import           HkHue.Client
import           HkHue.Config                   ( getConfig
                                                , defaultBindAddress
                                                , defaultBindPort
                                                )
import           HkHue.Messages                 ( ClientMsg(..)
                                                , DaemonMsg(..)
                                                , StateUpdate(..)
                                                , LightPower(..)
                                                , LightIdentifier(..)
                                                , LightData(..)
                                                , LightColor(..)
                                                )
import           HkHue.Types                    ( BridgeState(..)
                                                , BridgeLight(..)
                                                , BridgeLightState(..)
                                                )

import qualified Data.Map.Strict               as Map
import qualified Data.Text                     as T
import qualified Network.WebSockets            as WS

-- | Read the account file, start the sync thread & launch the WebSocket
-- server.
main :: IO ()
main = do
    config  <- getConfig
    account <- getHueAccount $ configBridgeHost config
    let hueConfig = HueConfig { hueBridgeHost = configBridgeHost config
                              , hueAccount    = account
                              }
        bridgeInterval = configBridgeSyncInterval config
        lightsInterval = configLightsSyncInterval config
    state <- newMVar
        $ initialDaemonState hueConfig (min bridgeInterval lightsInterval)
    bridgeSyncThread <- forkIO
        $ runReaderT (bridgeStateSync bridgeInterval) state
    lightsSyncThread <- forkIO
        $ runReaderT (lightStateSync lightsInterval) state
    let forkedThreads = [bridgeSyncThread, lightsSyncThread]
    flip finally (mapM_ killThread forkedThreads)
        $ WS.runServer (configBindHost config) (configBindPort config)
        $ application state


type App a = ReaderT (MVar DaemonState) IO a

readState :: App DaemonState
readState = ask >>= \s -> liftIO (readMVar s)

modifyState :: (DaemonState -> IO (DaemonState, a)) -> App a
modifyState f = ask >>= \s -> liftIO (modifyMVar s f)

modifyState_ :: (DaemonState -> IO DaemonState) -> App ()
modifyState_ f = ask >>= \s -> liftIO (modifyMVar_ s f)


data DaemonState =
        DaemonState { daemonHueConfig :: HueConfig
                    , daemonClients :: [(ClientId, WS.Connection)]
                    , daemonNextClientId :: ClientId
                    , daemonStoredBrightness :: Map.Map Int Int
                    , daemonStoredTemperature :: Map.Map Int Int
                    , daemonBridgeState :: BridgeState
                    , daemonCacheInterval :: Int
                    }

initialDaemonState :: HueConfig -> Int -> DaemonState
initialDaemonState hueConfig minSyncInterval = DaemonState
    { daemonHueConfig         = hueConfig
    , daemonClients           = []
    , daemonNextClientId      = ClientId 1
    , daemonStoredBrightness  = Map.empty
    , daemonStoredTemperature = Map.empty
    , daemonBridgeState       = BridgeState Map.empty
    , daemonCacheInterval     = minSyncInterval
    }


-- Websocket Client Identifiers
newtype ClientId = ClientId Integer deriving (Eq)

nextClientId :: App ClientId
nextClientId = modifyState $ \s ->
    let cId@(ClientId number) = daemonNextClientId s
    in  return (s { daemonNextClientId = ClientId (number + 1) }, cId)

-- | Convert a `LightIdentifier` into the bridge's Light ID using the
-- BridgeState to convert from Names to IDs.
fromLightIdentifier :: LightIdentifier -> App Int
fromLightIdentifier = \case
    LightId   i -> return i
    LightName n -> do
        lights <- bridgeLights . daemonBridgeState <$> readState
        case Map.foldrWithKey (findName n) Nothing lights of
            Just i  -> return i
            Nothing -> return 0
  where
    findName n i lightData = \case
        Just x  -> Just x
        Nothing -> if blName lightData == n then Just i else Nothing

-- | Handle a WS request, storing the Client ID & Connection.
application :: MVar DaemonState -> WS.ServerApp
application state pending = do
    conn <- WS.acceptRequest pending
    WS.withPingThread conn 30 (return ()) . flip runReaderT state $ do
        clientId <- nextClientId
        let client = (clientId, conn)
        flip finally (disconnect clientId) $ do
            modifyState_ $ \s ->
                return s { daemonClients = client : daemonClients s }
            forever $ eitherDecode' <$> liftIO (WS.receiveData conn) >>= \case
                Left errorString ->
                    sendDaemonMsg conn . ProtocolError $ T.pack errorString
                Right msg -> handleClientMessages client msg

-- | Remove the client from the client list
disconnect :: ClientId -> App ()
disconnect clientId = modifyState_ $ \s -> return s
    { daemonClients = filter ((/= clientId) . fst) $ daemonClients s
    }

-- | Make a request to the Hue API.
runHue :: HueClient a -> App a
runHue cmd = readState >>= liftIO . flip runClient cmd . daemonHueConfig

-- | Pull & update the `daemonBridgeState` every 60 seconds.
bridgeStateSync :: Int -> App ()
bridgeStateSync syncInterval =
    handleAny (const $ bridgeStateSync syncInterval) . forever $ do
        bridgeState <- runHue getFullBridgeState
        modifyState_ $ \s -> return s { daemonBridgeState = bridgeState }
        liftIO . threadDelay $ syncInterval * 1000000

-- | Pull & update the `bridgeLights` map every 5 seconds.
lightStateSync :: Int -> App ()
lightStateSync syncInterval =
    handleAny (const $ lightStateSync syncInterval) . forever $ do
        lightStates <- runHue getFullLightStates
        modifyState_ $ \s -> return s
            { daemonBridgeState = (daemonBridgeState s)
                { bridgeLights = lightStates
                }
            }
        liftIO . threadDelay $ syncInterval * 1000000

-- Client Messages

handleClientMessages :: (ClientId, WS.Connection) -> ClientMsg -> App ()
handleClientMessages (_, conn) = \case
    SetLightState lId lState -> do
        i <- fromLightIdentifier lId
        handlePowerBrightness i lState
            >>= storeColorTemperature i
            >>= runHue
            .   setState i
    SetLightName lId lName ->
        fromLightIdentifier lId >>= runHue . flip setName lName
    SetAllState lState -> everyLightState lState
    ResetAll           -> runHue resetColors
    Alert {..}         -> if null lightIds
        then runHue alertAll
        else mapM_ (fromLightIdentifier >=> runHue . alertLight) lightIds
    ScanLights          -> runHue searchForLights
    GetAverageColorTemp -> getAverageColorTemp conn
    GetLightInfo        -> getLightInfo conn
  where
    -- | Send an update to every light at once, unless a brightness
    -- adjustment is necessary(see `handlePowerBrightness`).
    everyLightState lState = do
        ids       <- Map.keys . bridgeLights . daemonBridgeState <$> readState
        newStates <- mapM
            (\i ->
                fmap (i, )
                    $   handlePowerBrightness i lState
                    >>= storeColorTemperature i
            )
            ids
        if any ((/=) lState . snd) newStates
            then forM_ newStates $ \(i, s) -> runHue $ setState i s
            else runHue $ setAllState lState

-- | Determine the average color temperature in Kelvins.
--
-- First we try using any powered-on lights that are in Color Temperature
-- mode. If there are none, we fall back to the temperature values we cache
-- during state updates. If there are none of those either, we simply wait
-- until the light state has been re-synced & then try again.
getAverageColorTemp :: WS.Connection -> App ()
getAverageColorTemp conn = do
    maybeCurrentTemp <- averageActiveLightTemperature
    maybeCachedTemp  <- averageCachedLightTemperature
    case maybeCurrentTemp <|> maybeCachedTemp of
        Nothing ->
            daemonCacheInterval
                <$> readState
                >>= liftIO
                .   threadDelay
                .   (* 1000000)
                >>  getAverageColorTemp conn
        Just ct -> sendDaemonMsg conn $ AverageColorTemp ct
  where
    -- Average the color temperature of lights that are turned on and in CT
    -- mode.
    averageActiveLightTemperature =
        fmap scaleColorTemp
            .   safeAvg
            .   map (\(_, _, colorTemp) -> colorTemp)
            .   filter (\(isOn, colorMode, _) -> isOn && colorMode == "ct")
            .   map ((\s -> (blsOn s, blsColorMode s, blsCT s)) . blState)
            .   Map.elems
            .   bridgeLights
            .   daemonBridgeState
            <$> readState

    -- Average the cached color temperatures.
    averageCachedLightTemperature =
        safeAvg . Map.elems . daemonStoredTemperature <$> readState

    -- Average a list, returning Nothing if it's empty.
    safeAvg xs = sum xs `safeDiv` length xs

-- | Send data for every light to the client.
getLightInfo :: WS.Connection -> App ()
getLightInfo conn =
    map buildLightData
        .   Map.assocs
        .   bridgeLights
        .   daemonBridgeState
        <$> readState
        >>= sendDaemonMsg conn
        .   LightInfo
  where
    buildLightData :: (Int, BridgeLight) -> LightData
    buildLightData (ident, light) =
        let state = blState light
            color = case blsColorMode state of
                "xy" -> RGBMode $ uncurry toRGB (blsXY state)
                _    -> CTMode . scaleColorTemp $ blsCT state
        in  LightData { ldId         = ident
                      , ldName       = blName light
                      , ldPower      = if blsOn state then On else Off
                      , ldColor      = color
                      , ldBrightness = unscaleBrightness $ blsBrightness state
                      }


sendDaemonMsg :: WS.Connection -> DaemonMsg -> App ()
sendDaemonMsg conn = liftIO . WS.sendTextData conn . encode

-- | Division, returning Nothing for divisors of 0.
safeDiv :: Integral a => a -> a -> Maybe a
n `safeDiv` d = if d == 0 then Nothing else Just $ n `div` d

--broadcastDaemonMsg :: MVar DaemonState -> DaemonMsg -> IO ()
--broadcastDaemonMsg state msg = do
--    clients <- daemonClients <$> readMVar state
--    forM_ clients $ \(_, conn) -> sendDaemonMsg conn msg



-- | When toggling the power off while setting a custom transition time,
-- the light(s) will transition to a brightness of 1 before turning
-- themselves off. If they are turned on with no brightness specified, they
-- will be at a brightness of 1(out of 254).
--
-- To avoid this, we check to see if a power off w/ transition is being
-- applied. If so, we store the specified brightness, or the current one if
-- unspecified. When a light is turned on, we clear out the stored
-- brightness and use it to set the light brightness if unspecified in the
-- `StateUpdate`.
handlePowerBrightness :: Int -> StateUpdate -> App StateUpdate
handlePowerBrightness lId update =
    case (suPower update, suTransitionTime update) of
        (Just Off, Just _) -> do
            storeBrightness =<< case suBrightness update of
                Just brightness -> return brightness
                Nothing ->
                    fmap fromIntegral
                        <$> runHue (getLightBrightness lId)
                        >>= \case
                                Nothing  -> return 1
                                Just bri -> return $ unscaleBrightness bri
            return update
        (Just On, _) -> do
            storedBrightness <- getAndDeleteBrightness
            case (suBrightness update, storedBrightness) of
                (Nothing, Just bri) ->
                    return $ update { suBrightness = Just bri }
                _ -> return update
        _ -> return update
  where
    storeBrightness bri = modifyState_ $ \s -> return s
        { daemonStoredBrightness = Map.insert lId bri $ daemonStoredBrightness s
        }
    getAndDeleteBrightness = modifyState $ \s ->
        let (bri, updatedMap) =
                    Map.updateLookupWithKey (\_ _ -> Nothing) lId
                        $ daemonStoredBrightness s
        in  return (s { daemonStoredBrightness = updatedMap }, bri)


-- | The `redshift` CLI command mode allows setting your monitor's color
-- temperature to match your lights using the redshift application. This
-- sources the color temperature of any lights currently in `ct` mode.
-- However, nothing will happen if your lights are in the `xy` or `hue/sat`
-- modes.
--
-- To avoid this, we store the color temperature of each light when we set
-- them. When calculating the average color temperature, we will source our
-- cached values if no powered-on lights are in `ct` mode.
--
-- Note that since xy values in updates will trump any ct values, we ignore
-- the case where both RGB & Kelvin colors are specified.
storeColorTemperature :: Int -> StateUpdate -> App StateUpdate
storeColorTemperature lightId update =
    case (suColor update, suColorTemperature update) of
        (Nothing, Just colorTemp) -> do
            modifyState_ $ \s -> return s
                { daemonStoredTemperature = Map.insert lightId colorTemp
                                                $ daemonStoredTemperature s
                }
            return update
        _ -> return update



-- Config File


data DaemonConfig =
    DaemonConfig { configBridgeHost :: T.Text
                 , configBindHost :: String
                 , configBindPort :: Int
                 , configBridgeSyncInterval :: Int
                 , configLightsSyncInterval :: Int
                 }

instance Default DaemonConfig where
    def = DaemonConfig { configBridgeHost         = "philips-hue"
                       , configBindHost           = defaultBindAddress
                       , configBindPort           = defaultBindPort
                       , configBridgeSyncInterval = 60
                       , configLightsSyncInterval = 5
                       }

instance FromJSON DaemonConfig where
    parseJSON = withObject "DaemonConfig" $ \o -> do
        daemonYaml <- o .: "daemon"
        DaemonConfig
            <$> (daemonYaml .: "bridge-host")
            <*> (o .: "bind-address")
            <*> (o .: "bind-port")
            <*> (daemonYaml .: "bridge-sync-interval")
            <*> (daemonYaml .: "lights-sync-interval")


-- Account Creation

-- | Either load the saved account or request a new account from the bridge
getHueAccount :: T.Text -> IO T.Text
getHueAccount bridgeHost = do
    accountFile <- getUserDataFile "hkhue" "account"
    hasAccount  <- isFile $ decodeString accountFile
    account     <- if hasAccount
        then T.pack <$> readFile accountFile
        else registerAccount bridgeHost
    unless hasAccount $ do
        getUserDataDir "hkhue" >>= createTree . decodeString
        writeFile accountFile $ T.unpack account
    return account

-- | Register a new account with the bridge. Prompts the user to press the
-- button on the bridge first.
registerAccount :: T.Text -> IO T.Text
registerAccount bridgeHost = do
    putStrLn "Please press the connection button on your bridge."
    putStrLn "Press [Enter] to continue"
    _ <- getLine
    registerWithBridge bridgeHost "hkhue" "cli" >>= \case
        Just acc -> do
            putStrLn . T.unpack $ "Successfully created account: " <> acc
            return acc
        Nothing -> do
            putStrLn "Encountered an error when registering with bridge.\n\n"
            registerAccount bridgeHost
