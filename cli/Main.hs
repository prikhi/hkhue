{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Main
    ( main
    )
where

import           Control.Concurrent             ( threadDelay )
import           Control.Exception.Safe         ( try )
import           Control.Monad                  ( forever
                                                , when
                                                )
import           Data.Data                      ( Data )
import           Data.Maybe                     ( fromMaybe )
import           Data.Typeable                  ( Typeable )
import           GHC.IO.Exception               ( IOException(ioe_type)
                                                , IOErrorType(NoSuchThing)
                                                )
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
                                                , details
                                                , helpArg
                                                , record
                                                , def
                                                , argPos
                                                , args
                                                , name
                                                , explicit
                                                , ignore
                                                , typ
                                                , summary
                                                )
import           Text.Read                      ( readMaybe )
import           Text.Tabular                   ( Table(..)
                                                , Header(..)
                                                , Properties(SingleLine)
                                                )
import           Text.Tabular.AsciiArt          ( render )
import           System.Exit                    ( ExitCode(ExitSuccess)
                                                , exitSuccess
                                                , exitFailure
                                                )
import           System.Process.Typed           ( readProcess
                                                , proc
                                                )

import           HkHue.Config                   ( ClientConfig(..)
                                                , getConfig
                                                )
import           HkHue.Messages                 ( ClientMsg(..)
                                                , DaemonMsg(..)
                                                , StateUpdate(..)
                                                , LightIdentifier(..)
                                                , RGBColor(..)
                                                , LightPower(..)
                                                , LightData(..)
                                                , sendMessage
                                                , receiveMessage
                                                )

import qualified Data.ByteString.Lazy          as L
import qualified Data.Text                     as T
import qualified Network.WebSockets            as WS


main :: IO ()
main = do
    clientMode <- cmdArgs_ arguments
    config     <- getConfig
    -- TODO: Catch resource vanished from Socket.recvBuf as well
    --       Also handle Socket.connect failures(e.g., no route to host)
    result     <- try . withSocketsDo $ WS.runClient
        (configDaemonAddress config)
        (configDaemonPort config)
        "/"
        (app $ dispatch clientMode)
    case result of
        Left WS.ConnectionClosed ->
            putStrLn "Lost Connection to Server, Retrying in 5 Seconds."
                >> threadDelay 5000000
                >> main
        Left err ->
            putStrLn ("Encountered a Connection Error: " <> show err)
                >> exitFailure
        Right () -> exitSuccess


-- WebSockets

type WSDispatch = WS.Connection -> IO ()

app :: WSDispatch -> WS.ClientApp ()
app socketConversation conn = do
    socketConversation conn
    WS.sendClose conn ("Quit" :: T.Text)

sendClientMsg :: WS.Connection -> ClientMsg -> IO ()
sendClientMsg = sendMessage

receiveDaemonMsg :: WS.Connection -> IO (Maybe DaemonMsg)
receiveDaemonMsg = receiveMessage


-- Command Modes

data ClientMode = SetLight
                    { lights :: [String]
                    , color :: Maybe RGBColor
                    , brightness :: Maybe Int
                    , colorTemperature :: Maybe Int
                    , transitionTime :: Maybe Int
                    , lightPower :: Maybe LightPower
                    , wait :: Bool
                    }
                | Rename
                    { light :: String
                    , lName :: String
                    }
                | AlertLight
                    { lights :: [String]
                    }
                | Reset
                | Scan
                | Redshift
                    { interval :: Int
                    }
                | Status
                deriving (Data, Typeable, Show, Eq)

parseLight :: String -> LightIdentifier
parseLight s = case readMaybe s of
    Nothing -> LightName $ T.pack s
    Just i  -> LightId i

dispatch :: ClientMode -> WSDispatch
dispatch = \case
    SetLight {..} -> \conn -> do
        let lightIds    = map parseLight lights
            stateUpdate = StateUpdate
                { suColor            = color
                , suBrightness       = brightness
                , suColorTemperature = colorTemperature
                , suTransitionTime   = transitionTime
                , suPower            = lightPower
                }
        if null lightIds
            then setAllState stateUpdate conn
            else setLightStates lightIds stateUpdate conn
        delayTransition wait transitionTime
    Rename {..} ->
        (`sendClientMsg` SetLightName (parseLight light) (T.pack lName))
    AlertLight {..} ->
        \conn -> sendClientMsg conn . Alert $ map parseLight lights
    Reset         -> (`sendClientMsg` ResetAll)
    Scan          -> (`sendClientMsg` ScanLights)
    Redshift {..} -> syncRedshift interval
    Status        -> printStatus
  where
    delayTransition wait transitionTime =
        when wait . threadDelay $ transitionDelayTime transitionTime
    -- | Convert a 100ms int into seconds for thread delay, adding a 0.5s
    -- padding to make up for bridge response times.
    transitionDelayTime maybeTime = fromMaybe 4 maybeTime * 100000 + 500000



-- Actions

setAllState :: StateUpdate -> WSDispatch
setAllState stateUpdate conn = sendClientMsg conn $ SetAllState stateUpdate

setLightStates :: [LightIdentifier] -> StateUpdate -> WSDispatch
setLightStates lightIds stateUpdate conn =
    mapM_ (sendClientMsg conn . flip SetLightState stateUpdate) lightIds

syncRedshift :: Int -> WSDispatch
syncRedshift syncInterval conn = forever $ do
    sendClientMsg conn GetAverageColorTemp
    receiveDaemonMsg conn >>= \case
        Just (AverageColorTemp ct) -> do
            try (readProcess (proc "redshift" ["-P", "-O", show ct])) >>= \case
                Right (exitCode, _, stderr) ->
                    when (exitCode /= ExitSuccess) $ L.putStr stderr
                Left e -> when (ioe_type e == NoSuchThing) $ putStrLn
                    "Error: Could not find `redshift` executable."
            threadDelay $ syncInterval * 1000000
        x -> putStrLn $ "Received Unexpected Message: " <> show x

printStatus :: WSDispatch
printStatus conn = do
    sendClientMsg conn GetLightInfo
    receiveDaemonMsg conn >>= \case
        Just (LightInfo info) -> putStrLn . render id id id $ infoToTable info
        x -> putStrLn $ "Received Unexpected Message: " <> show x
  where
    infoToTable info =
        Table
                (Group SingleLine $ map (Header . T.unpack . ldName) info)
                (Group
                    SingleLine
                    [ Header "ID"
                    , Header "Power"
                    , Header "Color"
                    , Header "Brightness"
                    ]
                )
            $ map
                  (\i ->
                      [ show $ ldId i
                      , show $ ldPower i
                      , show $ ldColor i
                      , show $ ldBrightness i
                      ]
                  )
                  info


-- Argument Parsing

arguments :: Annotate Ann
arguments =
    modes_ [status, setLights, setName, alert, reset, scan, redshift]
        += program "hkhue"
        += help "A scripting client for Philips Hue lights"
        += helpArg [name "h"]
        += summary "hkhue v0.1.0, GPL-3.0"

setLights :: Annotate Ann
setLights =
    record
            (SetLight def def def def def def def)
            [ lights := def += args += typ "LIGHT ..."
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
                [ atom (Nothing :: Maybe LightPower) += ignore
                , atom (Just On) += name "on" += help "Turn light on."
                , atom (Just Off) += name "off" += help "Turn light off."
                ]
            , wait := False += name "wait" += name "w" += explicit += help
                "Wait for transition to complete before exiting."
            ]
        += explicit
        += name "set"
        += help "Set the state of the light(s)."

setName :: Annotate Ann
setName =
    record
            (Rename def def)
            [ light := def += argPos 0 += typ "LIGHT"
            , lName := def += argPos 1 += typ "NAME"
            ]
        += explicit
        += name "rename"
        += help "Set the name of a specific light."

alert :: Annotate Ann
alert =
    record (AlertLight def) [lights := def += args += typ "LIGHT ..."]
        += explicit
        += name "alert"
        += help "Pulse the light(s), then return to the current state."

reset :: Annotate Ann
reset = record Reset [] += name "reset" += help
    "Reset all lights to the default color temperature of 2700K."

scan :: Annotate Ann
scan =
    record Scan []
        += name "scan"
        += help "Scan for new lights and add them to the bridge."
        += details
               [ "This mode tells the Hue bridge to scan for any unassociated "
                 <> "lights. While the command will exit immediately, the "
                 <> "bridge will continue to scan for 40 seconds & any new "
                 <> "lights will be added during daemon cache re-syncs."
               ]

redshift :: Annotate Ann
redshift =
    record
            (Redshift def)
            [ interval
              := 5
              += name "interval"
              += name "i"
              += explicit
              += typ "SECONDS"
              += help "Set the syncing interval."
            ]
        += name "redshift"
        += help "Sync redshift to your lights."
        += details
               [ "This mode starts a forever-running process that "
               <> "determines the average color temperature of all lights "
               <> "currently in `Color Temperature` mode & uses redshift to "
               <> "set your monitor's color temperature."
               , ""
               , "Note that the daemon uses it's local cache to calculate the "
               <> "average color temperature, effectively capping the lower "
               <> "bound of the `--interval` flag to the daemon's refresh "
               <> "interval."
               ]

status :: Annotate Ann
status = record Status [] += name "status" += help
    "Print the current status of the lights."
