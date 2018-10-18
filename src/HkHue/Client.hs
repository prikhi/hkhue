{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
module HkHue.Client
    ( HueConfig(..)
    , runClient
    , registerWithBridge
    , getLights
    , setState
    , setAllState
    , resetColors
    , scaleBrightness
    , scaleChannel
    , toXY
    )
where

import           Control.Lens                   ( (^?)
                                                --, (^@..)
                                                )
import           Control.Monad.Reader           ( ReaderT
                                                , runReaderT
                                                , asks
                                                , liftIO
                                                )
import           Data.Aeson                     ( (.=)
                                                , Value
                                                , object
                                                )
--import           Data.Aeson.Types               ( emptyArray )

import           Data.Aeson.Lens                ( key
                                                , nth
                                                --, members
                                                , _String
                                                , _JSON
                                                )
--import           Data.Maybe                     ( fromMaybe )
import           Data.Ratio                     ( (%) )
import           Network.Wreq

import           HkHue.Messages                 ( StateUpdate(..)
                                                , RGBColor(..)
                                                )

import qualified Data.Text                     as T

-- Config

data HueConfig =
    HueConfig
        { hueBridgeHost :: T.Text
        , hueAccount :: T.Text
        }

-- Client

type HueClient a = ReaderT HueConfig IO a

runClient :: HueConfig -> HueClient a -> IO a
runClient c m = runReaderT m c

-- Register

registerWithBridge :: T.Text -> T.Text -> T.Text -> IO (Maybe T.Text)
registerWithBridge bridgeHost applicationName deviceName = do
    response <- post (makeRequest bridgeHost "api")
        $ object ["devicetype" .= (applicationName <> "#" <> deviceName)]
    return
        $  response
        ^? responseBody
        .  nth 0
        .  key "success"
        .  key "username"
        .  _String

-- Lights

getLights :: HueClient (Maybe Value)
getLights = do
    response <- makeAuthRequest "lights" >>= liftIO . get
    liftIO (print response)
    return $ response ^? responseBody . _JSON

-- | Set the state of a light
setState :: Int -> StateUpdate -> HueClient ()
setState lightNumber stateUpdate = do
    response <-
        makeAuthRequest ("lights/" <> T.pack (show lightNumber) <> "/state")
        >>= liftIO
        .   (`put` stateUpdateToHueJSON stateUpdate)
    liftIO (print response)
    return ()


-- Groups

-- | Set the LightState of all connected lights.
-- TODO: Should be turn all lights on first?
setAllState :: StateUpdate -> HueClient ()
setAllState stateUpdate = do
    response <-
        makeAuthRequest "groups/0/action"
        >>= liftIO
        .   (`put` stateUpdateToHueJSON stateUpdate)
    liftIO $ print response
    return ()

-- | Switch all lights back to their factory default color
resetColors :: HueClient ()
resetColors = setAllState StateUpdate
    { suColor            = Nothing
    , suBrightness       = Just 100
    , suColorTemperature = Just 2732
    }


-- Color Utils

-- | Scale a Color Temperature from Kelvin to Reciprocal Megakelvin.
scaleColorTemp :: Int -> Int
scaleColorTemp ct = floor $ 1000000 % ct

-- | Scale a 1-100 Brightness value to 1-254
scaleBrightness :: Int -> Int
scaleBrightness b = floor (b % 100 * 254)

-- | Scale a 0-255 RGB channel to a 0-1 value.
scaleChannel :: Int -> Rational
scaleChannel c = toRational c / 255

-- | Convert RGB values from 0-1 into CIE XY values.
toXY :: Rational -> Rational -> Rational -> (Double, Double)
toXY r g b =
    let red   = gammaCorrect r
        green = gammaCorrect g
        blue  = gammaCorrect b
        x     = red * 0.664511 + green * 0.154324 + blue * 0.162028
        y     = red * 0.283881 + green * 0.668433 + blue * 0.047685
        z     = red * 0.000088 + green * 0.072310 + blue * 0.986039
        x'    = x / (x + y + z)
        y'    = y / (x + y + z)
    in  (x', y')
  where
    gammaCorrect c = if c > (4045 % 100000 :: Rational)
        then realToFrac ((c + (55 % 1000)) / (1 + (55 % 1000))) ** 2.4
        else fromRational $ c / (1292 % 100)


-- Request Utils


-- | Convert a `StateUpdate` type into the JSON the Hue API expects.
stateUpdateToHueJSON :: StateUpdate -> Value
stateUpdateToHueJSON StateUpdate {..} =
    object
        $  maybeValue
               (\(RGBColor r g b) -> "xy"
                   .= toXY (scaleChannel r) (scaleChannel g) (scaleChannel b)
               )
               suColor
        ++ maybeValue (\b -> "bri" .= scaleBrightness b) suBrightness
        ++ maybeValue (\ct -> "ct" .= scaleColorTemp ct) suColorTemperature
    where maybeValue f = maybe [] (\x -> [f x])


makeRequest :: T.Text -> T.Text -> String
makeRequest bridgeHost route =
    T.unpack $ "http://" <> bridgeHost <> "/" <> route

makeAuthRequest :: T.Text -> HueClient String
makeAuthRequest route = do
    bridgeHost <- asks hueBridgeHost
    account    <- asks hueAccount
    return . T.unpack $ T.concat
        ["http://", bridgeHost, "/api/", account, "/", route]
