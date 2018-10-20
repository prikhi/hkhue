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
                                                , atom
                                                , cmdArgs_
                                                , modes_
                                                , enum_
                                                , program
                                                , help
                                                , helpArg
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
                                                , LightPower(..)
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
                    , colorTemperature :: Maybe Int
                    , transitionTime :: Maybe Int
                    , lightPower :: Maybe LightPower
                    }
                | AlertLight
                    { light :: Int
                    }
                | SetAll
                    { color :: Maybe RGBColor
                    , brightness :: Maybe Int
                    , colorTemperature :: Maybe Int
                    , transitionTime :: Maybe Int
                    , lightPower :: Maybe LightPower
                    }
                | Reset
                deriving (Data, Typeable, Show, Eq)


dispatch :: ClientMode -> WSDispatch
dispatch = \case
    SetLight {..} -> setLightState
        light
        StateUpdate
            { suColor            = color
            , suBrightness       = brightness
            , suColorTemperature = colorTemperature
            , suTransitionTime   = transitionTime
            , suPower            = lightPower
            }
    AlertLight {..} -> (`sendClientMsg` Alert light)
    SetAll {..}     -> setAllState StateUpdate
        { suColor            = color
        , suBrightness       = brightness
        , suColorTemperature = colorTemperature
        , suTransitionTime   = transitionTime
        , suPower            = lightPower
        }
    Reset -> (`sendClientMsg` ResetAll)



-- Actions

setAllState :: StateUpdate -> WSDispatch
setAllState stateUpdate conn = sendClientMsg conn $ SetAllState stateUpdate

setLightState :: Int -> StateUpdate -> WSDispatch
setLightState lId stateUpdate conn =
    sendClientMsg conn $ SetLightState lId stateUpdate


-- Argument Parsing

arguments :: Annotate Ann
arguments =
    modes_ [setLight, alert, setAll, reset]
        += program "hkhue"
        += help "A scripting client for Philips Hue lights"
        += helpArg [name "h"]
        += summary "hkhue v0.1.0, GPL-3.0"

setLight :: Annotate Ann
setLight =
    record
            (SetLight def def def def def def)
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
            , colorTemperature
            := def
            += name "color-temperature"
            += name "ct"
            += name "k"
            += explicit
            += typ "INT"
            += help "Set the color temperature, in Kelvin, from 2000-6500."
            , transitionTime
            := def
            += name "transition-time"
            += name "t"
            += explicit
            += typ "INT"
            += help
                   ("Set the transition duration to the new state, in "
                   <> "100ms, from 0-65535. E.g., `-t 10` will set a transition "
                   <> "time of 1 second."
                   )
            , enum_
                lightPower
                [ atom (Just On) += name "on" += help "Turn light on."
                , atom (Just Off) += name "off" += help "Turn light off."
                ]
            ]
        += name "set-light"
        += help "Set the state of a specific light."

alert :: Annotate Ann
alert =
    record (AlertLight def) [light := def += argPos 0 += typ "LIGHT_ID"]
        += name "alert"
        += help "Toggle a specific light, then return to the current state."

setAll :: Annotate Ann
setAll =
    record
            (SetAll def def def def def)
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
            , colorTemperature
            := def
            += name "color-temperature"
            += name "ct"
            += name "k"
            += explicit
            += typ "INT"
            += help "Set the color temperature, in Kelvin, from 2000-6500."
            , transitionTime
            := def
            += name "transition-time"
            += name "t"
            += explicit
            += typ "INT"
            += help
                   ("Set the transition duration to the new state, in "
                   <> "100ms, from 0-65535. E.g., `-t 10` will set a transition "
                   <> "time of 1 second."
                   )
            , enum_
                lightPower
                [ atom (Just On) += name "on" += help "Turn light on."
                , atom (Just Off) += name "off" += help "Turn light off."
                ]
            ]
        += name "set-all"
        += help "Set the state of all lights."

reset :: Annotate Ann
reset = record Reset [] += name "reset" += help
    "Reset all lights to the default color temperature of 2700K."
