{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
module Main
    ( main
    )
where

import           Data.Aeson                     ( encode )
import           Data.Data                      ( Data )
import           Data.Typeable                  ( Typeable )
import           Network.Socket                 ( withSocketsDo )
import           System.Console.CmdArgs         ( Annotate(..)
                                                , Ann
                                                , (+=)
                                                , cmdArgs_
                                                , modes_
                                                , program
                                                , help
                                                , record
                                                , def
                                                , argPos
                                                , name
                                                , explicit
                                                , typ
                                                , summary
                                                )

import           HkHue.Messages                 ( ClientMsg(..)
                                                , StateUpdate(..)
                                                , RGBColor(..)
                                                )

import qualified Data.Text                     as T
import qualified Network.WebSockets            as WS


main :: IO ()
main = do
    clientMode <- cmdArgs_ arguments
    withSocketsDo
        $ WS.runClient "127.0.0.1" 9160 "/" (app $ dispatch clientMode)


-- WebSockets

type WSDispatch = WS.Connection -> IO ()

app :: WSDispatch -> WS.ClientApp ()
app socketConversation conn = do
    socketConversation conn
    WS.sendClose conn ("Quit" :: T.Text)

sendClientMsg :: WS.Connection -> ClientMsg -> IO ()
sendClientMsg conn = WS.sendTextData conn . encode


-- Command Modes

data ClientMode = SetLight
                    { light :: Int
                    , color :: Maybe RGBColor
                    , brightness :: Maybe Int
                    }
                | SetAll
                    { color :: Maybe RGBColor
                    , brightness :: Maybe Int
                    }
                deriving (Data, Typeable, Show, Eq)


dispatch :: ClientMode -> WSDispatch
dispatch = \case
    SetLight {..} -> setLightState light $ StateUpdate color brightness
    SetAll {..}   -> setAllState $ StateUpdate color brightness


-- Actions

setAllState :: StateUpdate -> WSDispatch
setAllState stateUpdate conn = sendClientMsg conn $ SetAllState stateUpdate

setLightState :: Int -> StateUpdate -> WSDispatch
setLightState lId stateUpdate conn =
    sendClientMsg conn $ SetLightState lId stateUpdate


-- Argument Parsing

arguments :: Annotate Ann
arguments =
    modes_ [setAll, setLight]
        += program "hkhue"
        += help "A scripting client for Philips Hue lights"
        += summary "hkhue v0.1.0, GPL-3.0"

setLight :: Annotate Ann
setLight =
    record
            (SetLight def def def)
            [ light := def += argPos 0 += typ "LIGHT_ID"
            , color
            := def
            += name "color"
            += name "c"
            += explicit
            += typ "RED,GREEN,BLUE"
            += help "Set the color using values from 0-255."
            , brightness
            := def
            += name "brightness"
            += name "b"
            += explicit
            += typ "INT"
            += help "Set the brightness using values from 1-254."
            ]
        += name "set-light"
        += help "Set the state of a specific light."

setAll :: Annotate Ann
setAll =
    record
            (SetAll def def)
            [ color
            := def
            += name "color"
            += name "c"
            += explicit
            += typ "RED,GREEN,BLUE"
            += help "Set the color using values from 0-255."
            , brightness
            := def
            += name "brightness"
            += name "b"
            += explicit
            += typ "INT"
            += help "Set the brightness using values from 1-254."
            ]
        += name "set-all"
        += help "Set the state of all lights."
