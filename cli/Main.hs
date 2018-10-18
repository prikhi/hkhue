{-# LANGUAGE OverloadedStrings #-}
module Main where

import           Data.Aeson                     ( encode )
import           Network.Socket                 ( withSocketsDo )

import           HkHue.Messages                 ( ClientMsg(..) )

import qualified Data.Text                     as T
import qualified Network.WebSockets            as WS

main :: IO ()
main = do
    -- TODO: Parse Args
    putStrLn "here"
    -- TODO: Use args to select websockets conversation
    withSocketsDo $ WS.runClient "127.0.0.1" 9160 "/" app

app :: WS.ClientApp ()
app conn = do
    sendClientMsg conn NoOp
    WS.sendClose conn ("Quit" :: T.Text)

sendClientMsg :: WS.Connection -> ClientMsg -> IO ()
sendClientMsg conn = WS.sendTextData conn . encode
