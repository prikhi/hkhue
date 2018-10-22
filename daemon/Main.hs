{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
module Main
    ( main
    )
where

import           Control.Concurrent             ( MVar
                                                , newMVar
                                                , readMVar
                                                , modifyMVar
                                                , modifyMVar_
                                                , threadDelay
                                                , forkIO
                                                , killThread
                                                )
import           Control.Monad                  ( unless
                                                , forever
                                                , forM_
                                                )
import           Control.Monad.Catch            ( finally )
import           Control.Monad.Reader           ( ReaderT
                                                , runReaderT
                                                , ask
                                                , liftIO
                                                )
import           Data.Aeson                     ( encode
                                                , eitherDecode'
                                                )
import           Filesystem                     ( isFile
                                                , createTree
                                                )
import           Filesystem.Path.CurrentOS      ( decodeString )
import           System.Environment             ( getArgs )
import           System.Environment.XDG.BaseDir ( getUserDataFile
                                                , getUserDataDir
                                                )
import           System.Exit                    ( exitFailure )

import           HkHue.Client
import           HkHue.Messages                 ( ClientMsg(..)
                                                , DaemonMsg(..)
                                                , StateUpdate(..)
                                                , LightPower(..)
                                                )
import           HkHue.Types                    ( BridgeState(..) )

import qualified Data.Map.Strict               as Map
import qualified Data.Text                     as T
import qualified Network.WebSockets            as WS

-- | Read the account file, start the sync thread & launch the WebSocket
-- server.
main :: IO ()
main = do
    -- TODO: If no known account, we send clients an "auth needed" message
    -- on connect, asking them to specify bridge host & press button to
    -- make account. That way daemon can start run without manual input.
    -- TODO: Bridge autodiscovery
    bridgeHost <- getArgs >>= \case
        []    -> putStrLn "Usage: hkhue <bridge-ip>" >> exitFailure
        b : _ -> return $ T.pack b
    account <- getHueAccount bridgeHost
    let config = HueConfig {hueBridgeHost = bridgeHost, hueAccount = account}
    state <-
        newMVar $ DaemonState config [] (ClientId 1) Map.empty $ BridgeState
            Map.empty
    bridgeSyncThread <- forkIO $ runReaderT bridgeStateSync state
    flip finally (killThread bridgeSyncThread)
        $ WS.runServer "0.0.0.0" 9160
        $ application state


type App a = ReaderT (MVar DaemonState) IO a

readState :: App DaemonState
readState = ask >>= \s -> liftIO (readMVar s)

modifyState :: (DaemonState -> IO (DaemonState, a)) -> App a
modifyState f = ask >>= \s -> liftIO (modifyMVar s f)

modifyState_ :: (DaemonState -> IO DaemonState) -> App ()
modifyState_ f = ask >>= \s -> liftIO (modifyMVar_ s f)


data DaemonState =
        DaemonState { daemonConfig :: HueConfig
                    , daemonClients :: [(ClientId, WS.Connection)]
                    , daemonNextClientId :: ClientId
                    , daemonStoredBrightness :: Map.Map Int Int
                    , daemonBridgeState :: BridgeState
                    }


-- Websocket Client Identifiers
newtype ClientId = ClientId Integer deriving (Eq)

nextClientId :: App ClientId
nextClientId = modifyState $ \s ->
    let cId@(ClientId number) = daemonNextClientId s
    in  return (s { daemonNextClientId = ClientId (number + 1) }, cId)

-- | Handle a WS request, storing the Client ID & Connection.
application :: MVar DaemonState -> WS.ServerApp
application state pending = flip runReaderT state $ do
    conn <- liftIO $ WS.acceptRequest pending
    liftIO $ WS.forkPingThread conn 30
    clientId <- nextClientId
    let client = (clientId, conn)
    flip finally (disconnect clientId) $ do
        modifyState_
            $ \s -> return s { daemonClients = client : daemonClients s }
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
runHue cmd = readState >>= liftIO . flip runClient cmd . daemonConfig

-- | Pull & update the `daemonBridgeState` every 60 seconds.
bridgeStateSync :: App ()
bridgeStateSync = forever $ do
    bridgeState <- runHue getFullBridgeState
    modifyState_ $ \s -> return s { daemonBridgeState = bridgeState }
    liftIO . threadDelay $ 60 * 1000000


-- Client Messages

handleClientMessages :: (ClientId, WS.Connection) -> ClientMsg -> App ()
handleClientMessages _ = \case
    SetLightState lId lState ->
        handlePowerBrightness lId lState >>= runHue . setState lId
    SetLightName lId lName -> runHue $ setName lId lName
    SetAllState lState     -> everyLightState lState
    ResetAll               -> runHue resetColors
    Alert lId              -> runHue $ alertLight lId
  where
    -- | Send an update to every light at once, unless a brightness
    -- adjustment is necessary(see `handlePowerBrightness`).
    -- TODO: Use daemonBridgeState to get light ids
    everyLightState lState = do
        ids       <- Map.keys . bridgeLights . daemonBridgeState <$> readState
        newStates <- mapM (\i -> (i, ) <$> handlePowerBrightness i lState) ids
        if any ((/=) lState . snd) newStates
            then forM_ newStates $ \(i, s) -> (runHue $ setState i s)
            else runHue $ setAllState lState


sendDaemonMsg :: WS.Connection -> DaemonMsg -> App ()
sendDaemonMsg conn = liftIO . WS.sendTextData conn . encode

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
