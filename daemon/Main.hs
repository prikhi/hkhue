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
import           Control.Exception              ( finally )
import           Control.Monad                  ( unless
                                                , forever
                                                , forM_
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
    bridgeSyncThread <- forkIO $ bridgeStateSync state
    flip finally (killThread bridgeSyncThread)
        $ WS.runServer "0.0.0.0" 9160
        $ application state


data DaemonState =
        DaemonState { daemonConfig :: HueConfig
                    , daemonClients :: [(ClientId, WS.Connection)]
                    , daemonNextClientId :: ClientId
                    , daemonStoredBrightness :: Map.Map Int Int
                    , daemonBridgeState :: BridgeState
                    }


-- Websocket Client Identifiers
newtype ClientId = ClientId Integer deriving (Eq)

nextClientId :: MVar DaemonState -> IO ClientId
nextClientId state = modifyMVar state $ \s ->
    let cId@(ClientId number) = daemonNextClientId s
    in  return (s { daemonNextClientId = ClientId (number + 1) }, cId)

-- | Handle a WS request
application :: MVar DaemonState -> WS.ServerApp
application state pending = do
    conn <- WS.acceptRequest pending
    WS.forkPingThread conn 30
    nextId <- nextClientId state
    let client = (nextId, conn)
    flip finally (disconnect state nextId) $ do
        modifyMVar_ state
            $ \s -> return s { daemonClients = client : daemonClients s }
        forever $ eitherDecode' <$> WS.receiveData conn >>= \case
            Left errorString ->
                sendDaemonMsg conn . ProtocolError $ T.pack errorString
            Right msg -> handleClientMessages state client msg

-- | Remove the client from the client list
disconnect :: MVar DaemonState -> ClientId -> IO ()
disconnect state clientId = modifyMVar_ state $ \s -> return s
    { daemonClients = filter ((/= clientId) . fst) $ daemonClients s
    }

-- | Make a request to the Hue API.
runHue :: MVar DaemonState -> HueClient a -> IO a
runHue state cmd = daemonConfig <$> readMVar state >>= flip runClient cmd

-- | Pull & update the `daemonBridgeState` every 60 seconds.
bridgeStateSync :: MVar DaemonState -> IO ()
bridgeStateSync state = forever $ do
    bridgeState <- runHue state getFullBridgeState
    modifyMVar_ state $ \s -> return s { daemonBridgeState = bridgeState }
    threadDelay $ 60 * 1000000


-- Client Messages

handleClientMessages
    :: MVar DaemonState -> (ClientId, WS.Connection) -> ClientMsg -> IO ()
handleClientMessages state _ = \case
    SetLightState lId lState ->
        handlePowerBrightness state lId lState >>= runHue state . setState lId
    SetLightName lId lName -> runHue state $ setName lId lName
    SetAllState lState     -> everyLightState lState
    ResetAll               -> runHue state resetColors
    Alert lId              -> runHue state $ alertLight lId
  where
    -- | Send an update to every light at once, unless a brightness
    -- adjustment is necessary(see `handlePowerBrightness`).
    everyLightState lState = do
        ids <- Map.keys . bridgeLights . daemonBridgeState <$> readMVar state
        newStates <- mapM
            (\i -> (i, ) <$> handlePowerBrightness state i lState)
            ids
        if any ((/=) lState . snd) newStates
            then forM_ newStates $ \(i, s) -> (runHue state $ setState i s)
            else runHue state $ setAllState lState


sendDaemonMsg :: WS.Connection -> DaemonMsg -> IO ()
sendDaemonMsg conn = WS.sendTextData conn . encode

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
handlePowerBrightness
    :: MVar DaemonState -> Int -> StateUpdate -> IO StateUpdate
handlePowerBrightness state lId update =
    case (suPower update, suTransitionTime update) of
        (Just Off, Just _) -> do
            storeBrightness =<< case suBrightness update of
                Just brightness -> return brightness
                Nothing ->
                    fmap fromIntegral
                        <$> runHue state (getLightBrightness lId)
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
    storeBrightness bri = modifyMVar_ state $ \s -> return s
        { daemonStoredBrightness = Map.insert lId bri $ daemonStoredBrightness s
        }
    getAndDeleteBrightness = modifyMVar state $ \s ->
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
