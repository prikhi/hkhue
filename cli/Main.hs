{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE RecordWildCards #-}
module Main
    ( main
    )
where

import           Data.Aeson                     ( encode
                                                , object
                                                , (.=)
                                                )
import           Data.Data                      ( Data )
import           Data.Ratio                     ( (%) )
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

import           HkHue.Client                   ( scaleChannel
                                                , scaleBrightness
                                                , toXY
                                                )
import           HkHue.Messages                 ( ClientMsg(..) )

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

-- Modes -> WebSockets

data ClientMode = SetBrightness { light :: Int, bright :: Int }
                | SetAll { saColor :: Maybe SAColor, saBrightness :: Maybe Int }
                deriving (Data, Typeable, Show, Eq)

data SAColor = SAColor { sacRed :: Int, sacGreen :: Int, sacBlue :: Int }
                deriving (Data, Typeable, Show, Eq)

dispatch :: ClientMode -> WSDispatch
dispatch = \case
    SetBrightness l b -> setLightBrightness l (floor $ b % 100 * 255)
    SetAll {..}       -> setAllState saColor saBrightness

setAllState :: Maybe SAColor -> Maybe Int -> WSDispatch
setAllState mColor mBrightness conn
    = let values =
              maybeValue
                      (\(SAColor r g b) -> "xy" .= toXY (scaleChannel r)
                                                        (scaleChannel g)
                                                        (scaleChannel b)
                      )
                      mColor
                  ++ maybeValue (\b -> "bri" .= scaleBrightness b) mBrightness
      in  if null values
              then putStrLn
                  "Specify at least one flag to update the light states."
              else sendClientMsg conn $ SetAllState $ object values
    where maybeValue f = maybe [] (\x -> [f x])

setLightBrightness :: Int -> Int -> WSDispatch
setLightBrightness lId brightness conn = sendClientMsg conn
    $ SetLightState lId lightData
    where lightData = object ["bri" .= brightness]


-- Argument Parsing

arguments :: Annotate Ann
arguments =
    modes_ [setBrightness, setAll]
        += program "hkhue"
        += help "A scripting client for Philips Hue lights"
        += summary "hkhue v0.1.0, GPL-3.0"

setAll :: Annotate Ann
setAll =
    record
            (SetAll def def)
            [ saColor
            := def
            += name "color"
            += name "c"
            += explicit
            += typ "RED,GREEN,BLUE"
            += help "Set the color using values from 0-255."
            , saBrightness
            := def
            += name "brightness"
            += name "b"
            += typ "INT"
            += help "Set the brightness using values from 1-254."
            ]
        += name "set-all"
        += help "Set the state of all lights."

setBrightness :: Annotate Ann
setBrightness =
    record
            SetBrightness {light = def, bright = def}
            [ light := def += argPos 0 += typ "LIGHT_ID"
            , bright := def += argPos 1 += typ "BRIGHTNESS"
            ]
        += name "set-brightness"
        += help "Set the Brightness(0-254) of a light."
