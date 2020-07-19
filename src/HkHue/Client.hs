{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
module HkHue.Client
    ( HueConfig(..)
    , HueClient
    , runClient
    , registerWithBridge
    , getLights
    , getLightBrightness
    , alertLight
    , setState
    , setName
    , searchForLights
    , alertAll
    , setAllState
    , resetColors
    , getGroups
    , createGroup
    , renameGroup
    , setGroupState
    , getFullBridgeState
    , getFullLightStates
    , scaleColorTemp
    , scaleBrightness
    , unscaleBrightness
    , scaleChannel
    , toXY
    , toRGB
    )
where

import           Control.Lens                   ( (^?)
                                                , (^.)
                                                )
import           Control.Monad.Reader           ( ReaderT
                                                , runReaderT
                                                , asks
                                                , liftIO
                                                )
import           Data.Aeson                     ( (.=)
                                                , FromJSON
                                                , Value
                                                , object
                                                )
import           Data.Aeson.Lens                ( key
                                                , nth
                                                , _String
                                                , _Integer
                                                , _JSON
                                                )
import           Data.Ratio                     ( (%) )
import           Network.Wreq

import           HkHue.Messages                 ( StateUpdate(..)
                                                , RGBColor(..)
                                                , LightPower(..)
                                                )
import           HkHue.Types                    ( BridgeState(..)
                                                , BridgeLight
                                                )

import qualified Data.Map.Strict               as Map
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

-- | Return the Brightness of a light, from 1-254.
getLightBrightness :: Int -> HueClient (Maybe Integer)
getLightBrightness lId = do
    response <-
        makeAuthRequest ("lights/" <> T.pack (show lId)) >>= liftIO . get
    liftIO $ print response
    return $ response ^? responseBody . key "state" . key "bri" . _Integer

-- | Use the alert effect to cycle one light on/off.
alertLight :: Int -> HueClient ()
alertLight lightNumber = do
    response <-
        makeAuthRequest ("lights/" <> T.pack (show lightNumber) <> "/state")
        >>= liftIO
        .   (`put` object ["alert" .= ("select" :: T.Text)])
    liftIO (print response)

-- | Set the state of a light
setState :: Int -> StateUpdate -> HueClient ()
setState lightNumber stateUpdate = do
    response <-
        makeAuthRequest ("lights/" <> T.pack (show lightNumber) <> "/state")
        >>= liftIO
        .   (`put` stateUpdateToHueJSON stateUpdate)
    liftIO (print response)

-- | Set the name of a light
setName :: Int -> T.Text -> HueClient ()
setName lightNumber newName = do
    response <-
        makeAuthRequest ("lights/" <> T.pack (show lightNumber))
        >>= liftIO
        .   (`put` object ["name" .= newName])
    liftIO (print response)

-- | Scan for new lights & assign them to the bridge
searchForLights :: HueClient ()
searchForLights =
    makeAuthRequest "lights"
        >>= liftIO
        .   flip post ([] :: [FormParam])
        >>= liftIO
        .   print

-- Groups

alertAll :: HueClient ()
alertAll =
    makeAuthRequest "groups/0/action"
        >>= liftIO
        .   (`put` object ["alert" .= ("select" :: T.Text)])
        >>= liftIO
        .   print

-- | Set the LightState of all connected lights.
setAllState :: StateUpdate -> HueClient ()
setAllState stateUpdate = do
    response <-
        makeAuthRequest "groups/0/action"
        >>= liftIO
        .   (`put` stateUpdateToHueJSON stateUpdate)
    liftIO $ print response

-- | Switch all lights back to their factory default color
resetColors :: HueClient ()
resetColors = setAllState StateUpdate { suColor            = Nothing
                                      , suBrightness       = Just 100
                                      , suColorTemperature = Just 2732
                                      , suTransitionTime   = Nothing
                                      , suPower            = Just On
                                      }

-- | Get a list of all groups added to the bridge.
getGroups :: HueClient (Maybe Value)
getGroups = do
    response <- makeAuthRequest "groups" >>= liftIO . get
    liftIO (print response)
    return $ response ^? responseBody . _JSON

-- | Create a group with the given name and optional set of light IDs.
createGroup :: T.Text -> [Int] -> HueClient (Maybe Integer)
createGroup name lightIds = do
    let req = object
            [ "lights" .= map show lightIds
            , "name" .= name
            , "type" .= ("LightGroup" :: T.Text)
            ]
    response <- makeAuthRequest "groups" >>= liftIO . (`post` req)
    liftIO (print response)
    return
        $  response
        ^? responseBody
        .  nth 0
        .  key "success"
        .  key "id"
        .  _Integer

renameGroup :: Int -> T.Text -> HueClient ()
renameGroup groupId newName =
    let req = object ["name" .= newName]
    in  makeAuthRequest ("groups/" <> T.pack (show groupId))
            >>= liftIO
            .   (`put` req)
            >>= liftIO
            .   print

setGroupState :: Int -> StateUpdate -> HueClient ()
setGroupState groupId stateUpdate =
    makeAuthRequest ("groups/" <> T.pack (show groupId) <> "/action")
        >>= liftIO
        .   (`put` stateUpdateToHueJSON stateUpdate)
        >>= liftIO
        .   print


-- Configuration

-- | Pull the entire `BridgeState` from the Hue bridge.
getFullBridgeState :: HueClient BridgeState
getFullBridgeState = getPrintAndDecode ""

-- | Pull all `BridgeLight` states from the Hue bridge.
getFullLightStates :: HueClient (Map.Map Int BridgeLight)
getFullLightStates = getPrintAndDecode "lights"

-- | Make an authorized GET request, print the response, & decode the body.
getPrintAndDecode :: FromJSON a => T.Text -> HueClient a
getPrintAndDecode path = do
    response <- makeAuthRequest path >>= liftIO . get
    liftIO $ print response
    (^. responseBody) <$> asJSON response


-- Color Utils

-- | Scale a Color Temperature from Kelvin to Reciprocal Megakelvin.
scaleColorTemp :: Int -> Int
scaleColorTemp ct = floor $ 1000000 % ct

-- | Scale a 1-100 Brightness value to 1-254
scaleBrightness :: Int -> Int
scaleBrightness b = floor (b % 100 * 254)

-- | Scale a 1-254 Brightness value to 1-100
unscaleBrightness :: Int -> Int
unscaleBrightness b = ceiling (b * 100 % 254)

-- | Scale a 0-255 RGB channel to a 0-1 value.
scaleChannel :: Int -> Rational
scaleChannel c = toRational c / 255

-- | Convert RGB values from 0-1 into CIE XY values.
-- See:
-- https://developers.meethue.com/documentation/color-conversions-rgb-xy
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

-- | Convert CIE XY values to RGB values.
-- See:
-- https://developers.meethue.com/documentation/color-conversions-rgb-xy
toRGB :: Rational -> Rational -> RGBColor
toRGB x' y' =
    let z' = 1.0 - x' - y'
        y  = 1.0
        x  = y / y' * x'
        z  = y / y' * z'
        r  = x * 1.656492 - y * 0.354851 - z * 0.255038
        g  = x * (-0.707196) + y * 1.655397 + z * 0.036152
        b  = x * 0.051713 - y * 0.121364 + z * 1.011530
        (red, green, blue) =
                map3 (round . (* 255)) $ map3 reverseGamma $ toUnitRGB r g b
    in  RGBColor red green blue
  where
    --gammaAndScale c = round $ 255 * c
    map3 f (a, b, c) = (f a, f b, f c)
    toUnitRGB r g b | r > b && r > g && r > 1 = (1, g / r, b / r)
                    | g > b && g > r && g > 1 = (r / g, 1, b / g)
                    | b > g && b > r && b > 1 = (r / b, g / b, 1)
                    | otherwise               = (r, g, b)
    reverseGamma :: Rational -> Rational
    reverseGamma c = if c <= 0.0031308
        then 12.92 * c
        else 1.055 * toRational (realToFrac c ** (1 / 2.4) :: Double) - 0.055



-- Request Utils


-- | Convert a `StateUpdate` type into the state JSON the Hue API expects.
stateUpdateToHueJSON :: StateUpdate -> Value
stateUpdateToHueJSON StateUpdate {..} =
    object
        $  maybeField
               "xy"
               (\(RGBColor r g b) ->
                   toXY (scaleChannel r) (scaleChannel g) (scaleChannel b)
               )
               suColor
        ++ maybeField "bri"            scaleBrightness suBrightness
        ++ maybeField "ct"             scaleColorTemp  suColorTemperature
        ++ maybeField "transitiontime" id              suTransitionTime
        ++ maybeField "on"             (On ==)         suPower
    where maybeField k f = maybe [] (\x -> [k .= f x])


makeRequest :: T.Text -> T.Text -> String
makeRequest bridgeHost route =
    T.unpack $ "http://" <> bridgeHost <> "/" <> route

makeAuthRequest :: T.Text -> HueClient String
makeAuthRequest route = do
    bridgeHost <- asks hueBridgeHost
    account    <- asks hueAccount
    return . T.unpack $ T.concat
        ["http://", bridgeHost, "/api/", account, "/", route]
