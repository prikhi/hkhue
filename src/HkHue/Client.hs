{-# LANGUAGE OverloadedStrings #-}
module HkHue.Client
    ( HueConfig(..)
    , runClient
    , registerWithBridge
    , getLights
    , setColor
    , setAllColor
    , resetColors
    )
where

import           Control.Lens                   ( (^?)
                                                , (^@..)
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
import           Data.Aeson.Types               ( emptyArray )

import           Data.Aeson.Lens                ( key
                                                , nth
                                                , members
                                                , _String
                                                , _JSON
                                                )
import           Data.Maybe                     ( fromMaybe )
import           Data.Ratio                     ( (%) )
import           Network.Wreq

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

-- | Switch all lights back to their factory default color
resetColors :: HueClient ()
resetColors = do
    lightCount <- length . (^@.. members) . fromMaybe emptyArray <$> getLights
    let defaultColor = object
            [ "colormode" .= ("ct" :: T.Text)
            , "ct" .= (366 :: Integer)
            , "bri" .= (254 :: Integer)
            ]
    mapM_ (`setState` defaultColor) [1 .. lightCount]

-- | TODO: Ensure light is on - for performance reasons we don't always
-- send "turn on" command
setColor :: Int -> Int -> Int -> Int -> HueClient ()
setColor lightNumber r g b =
    let (x, y) = toXY (toRational r / 255.0)
                      (toRational g / 255.0)
                      (toRational b / 255.0)
    in  setState lightNumber $ object ["xy" .= [x, y]]

-- | Set the state of a light
setState :: Int -> Value -> HueClient ()
setState lightNumber stateJSON = do
    response <-
        makeAuthRequest ("lights/" <> T.pack (show lightNumber) <> "/state")
        >>= liftIO
        .   (`put` stateJSON)
    liftIO (print response)
    return ()


-- Groups

setAllState :: Value -> HueClient ()
setAllState stateJSON = do
    response <- makeAuthRequest "groups/0/action" >>= liftIO . (`put` stateJSON)
    liftIO $ print response
    return ()

setAllColor :: Int -> Int -> Int -> HueClient ()
setAllColor r g b =
    let (x, y) = toXY (toRational r / 255.0)
                      (toRational g / 255.0)
                      (toRational b / 255.0)
    in  setAllState $ object ["xy" .= [x, y], "on" .= True]


-- Color Utils

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

makeRequest :: T.Text -> T.Text -> String
makeRequest bridgeHost route =
    T.unpack $ "http://" <> bridgeHost <> "/" <> route

makeAuthRequest :: T.Text -> HueClient String
makeAuthRequest route = do
    bridgeHost <- asks hueBridgeHost
    account    <- asks hueAccount
    return . T.unpack $ T.concat
        ["http://", bridgeHost, "/api/", account, "/", route]
