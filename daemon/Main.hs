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
                                                )
import           Control.Exception              ( finally )
import           Control.Lens                   ( (^@..) )
import           Control.Monad                  ( unless
                                                , forever
                                                , forM_
                                                )
import           Data.Aeson                     ( encode
                                                , eitherDecode'
                                                )
import           Data.Aeson.Lens                ( members )
import           Data.Maybe                     ( mapMaybe )
import           Filesystem                     ( isFile
                                                , createTree
                                                )
import           Filesystem.Path.CurrentOS      ( decodeString )
import           System.Environment             ( getArgs )
import           System.Environment.XDG.BaseDir ( getUserDataFile
                                                , getUserDataDir
                                                )
import           System.Exit                    ( exitFailure )
import           Text.Read                      ( readMaybe )

import           HkHue.Client
import           HkHue.Messages                 ( ClientMsg(..)
                                                , DaemonMsg(..)
                                                , StateUpdate(..)
                                                , LightPower(..)
                                                )

import qualified Data.Map.Strict               as Map
import qualified Data.Text                     as T
import qualified Network.WebSockets            as WS

main :: IO ()
main = do
    -- TODO: If no known account, we send clients an "auth needed" message
    -- on connect, asking them to specify bridge host & press button to
    -- make account. That way daemon can start run without manual input.
    bridgeHost <- getArgs >>= \case
        []    -> putStrLn "Usage: hkhue <bridge-ip>" >> exitFailure
        b : _ -> return $ T.pack b
    account <- getHueAccount bridgeHost
    let config = HueConfig {hueBridgeHost = bridgeHost, hueAccount = account}
    state <- newMVar $ DaemonState config [] (ClientId 1) Map.empty
    WS.runServer "0.0.0.0" 9160 $ application state

data DaemonState =
        DaemonState { daemonConfig :: HueConfig
                    , daemonClients :: [(ClientId, WS.Connection)]
                    , daemonNextClientId :: ClientId
                    , daemonStoredBrightness :: Map.Map Int Int
                    }

newtype ClientId = ClientId Integer deriving (Eq)
nextClientId :: MVar DaemonState -> IO ClientId
nextClientId state = modifyMVar state $ \s ->
    let cId@(ClientId number) = daemonNextClientId s
    in  return (s { daemonNextClientId = ClientId (number + 1) }, cId)



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


-- Client Messages

handleClientMessages
    :: MVar DaemonState -> (ClientId, WS.Connection) -> ClientMsg -> IO ()
handleClientMessages state _ = \case
    SetLightState lId lState ->
        handlePowerBrightness state lId lState >>= run . setState lId
    SetLightName lId lName -> run $ setName lId lName
    SetAllState lState     -> everyLightState lState
    ResetAll               -> run resetColors
    Alert lId              -> run $ alertLight lId
  where
    run cmd = daemonConfig <$> readMVar state >>= flip runClient cmd
    -- | Send an update to every light at once, unless a brightness
    -- adjustment is necessary(see `handlePowerBrightness`).
    everyLightState lState = do
        mValue <- run getLights
        let ids = case mValue of
                Just v ->
                    mapMaybe (readMaybe . T.unpack . fst) $ v ^@.. members
                Nothing -> []
        newStates <- mapM
            (\i -> (i, ) <$> handlePowerBrightness state i lState)
            ids
        if any ((/=) lState . snd) newStates
            then forM_ newStates $ \(i, s) -> (run $ setState i s)
            else run $ setAllState lState


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
                    fmap fromIntegral <$> run (getLightBrightness lId) >>= \case
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
    run cmd = daemonConfig <$> readMVar state >>= flip runClient cmd
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
