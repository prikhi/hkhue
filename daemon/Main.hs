{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
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
import           Control.Monad                  ( unless
                                                , forever
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
                                                )

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
    state <- newMVar $ DaemonState config [] (ClientId 1)
    WS.runServer "0.0.0.0" 9160 $ application state

data DaemonState =
        DaemonState { daemonConfig :: HueConfig
                    , daemonClients :: [(ClientId, WS.Connection)]
                    , daemonNextClientId :: ClientId}

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
    SetLightState lId lState -> run $ setState lId lState
    SetAllState lState       -> run $ setAllState lState
    ResetAll                 -> run resetColors
    Alert lId                -> run $ alertLight lId
    where run cmd = daemonConfig <$> readMVar state >>= flip runClient cmd

sendDaemonMsg :: WS.Connection -> DaemonMsg -> IO ()
sendDaemonMsg conn = WS.sendTextData conn . encode

--broadcastDaemonMsg :: MVar DaemonState -> DaemonMsg -> IO ()
--broadcastDaemonMsg state msg = do
--    clients <- daemonClients <$> readMVar state
--    forM_ clients $ \(_, conn) -> sendDaemonMsg conn msg


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
