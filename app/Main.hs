{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
module Main where

import           Control.Monad                  ( unless )
import           Control.Monad.IO.Class         ( liftIO )
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

import qualified Data.Text                     as T

main :: IO ()
main = do
    bridgeHost <- getArgs >>= \case
        []    -> putStrLn "Usage: hkhue <bridge-ip>" >> exitFailure
        b : _ -> return $ T.pack b
    account <- getHueAccount bridgeHost
    let config = HueConfig {hueBridgeHost = bridgeHost, hueAccount = account}
    -- TODO: run websockets server
    runClient config $ do
        getLights >>= liftIO . print
        resetColors

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
