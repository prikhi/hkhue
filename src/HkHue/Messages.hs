{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
module HkHue.Messages
    ( ClientMsg(..)
    , DaemonMsg(..)
    , LightIdentifier(..)
    , RGBColor(..)
    , LightPower(..)
    , StateUpdate(..)
    , LightData(..)
    , LightColor(..)
    , sendMessage
    , receiveMessage
    )
where

import           Control.Monad.IO.Class         ( MonadIO
                                                , liftIO
                                                )
import           Data.Aeson                     ( FromJSON
                                                , ToJSON(..)
                                                , encode
                                                , decode
                                                , genericToEncoding
                                                , defaultOptions
                                                )
import           Data.Data                      ( Data )
import           Data.Default                   ( Default(..) )
import           Data.Typeable                  ( Typeable )
import           GHC.Generics

import qualified Data.Text                     as T
import qualified Network.WebSockets            as WS

-- Client Messages

data ClientMsg
    = SetLightState { lightId :: LightIdentifier, lightState :: StateUpdate }
    | SetLightName { lightId :: LightIdentifier, lightName :: T.Text }
    | SetAllState { lightState :: StateUpdate }
    | ResetAll
    | Alert { lightIds :: [LightIdentifier] }
    | ScanLights
    | GetAverageColorTemp
    | GetLightInfo
    deriving (Generic, Show)


-- Daemon Messages

data DaemonMsg
    = ProtocolError T.Text
    | AverageColorTemp Int
    | LightInfo [LightData]
    deriving (Generic, Show)

-- Message Helpers

-- | Generic WebSocket Message Sending. The client & daemon should use this
-- to build functions with more specific types.
sendMessage :: (ToJSON a, MonadIO m) => WS.Connection -> a -> m ()
sendMessage conn = liftIO . WS.sendTextData conn . encode

-- | Generic WebSocket Message Receiving. Specialize the types in the
-- client & daemons.
receiveMessage :: (FromJSON a, MonadIO m) => WS.Connection -> m (Maybe a)
receiveMessage conn = liftIO $ decode <$> WS.receiveData conn

-- Accessory Types

data LightIdentifier
    = LightId Int
    | LightName T.Text
    deriving (Data, Typeable, Generic, Show, Eq)

data RGBColor
    = RGBColor
        { cRed :: Int
        , cGreen :: Int
        , cBlue :: Int
        } deriving (Data, Typeable, Generic, Show, Eq)

data LightPower
    = On
    | Off
    deriving (Data, Typeable, Generic, Show, Eq)

data StateUpdate
    = StateUpdate
        { suColor :: Maybe RGBColor
        , suBrightness :: Maybe Int
        , suColorTemperature :: Maybe Int
        , suTransitionTime :: Maybe Int
        , suPower :: Maybe LightPower
        } deriving (Data, Typeable, Generic, Show, Eq)

instance Default StateUpdate where
    def = StateUpdate Nothing Nothing Nothing Nothing Nothing


data LightData
    = LightData
        { ldId :: Int
        , ldName :: T.Text
        , ldPower :: LightPower
        , ldColor :: LightColor
        , ldBrightness :: Int
        } deriving (Data, Typeable, Generic, Show, Eq)

data LightColor
    = RGBMode RGBColor
    | CTMode Int
    deriving (Data, Typeable, Generic, Eq)

instance Show LightColor where
    show (RGBMode (RGBColor r g b)) =
        concat ["(", show r, ", ", show g, ", ", show b, ")"]
    show (CTMode i) = show i <> "K"



-- Aeson Classes

instance FromJSON ClientMsg
instance ToJSON ClientMsg where
    toEncoding = genericToEncoding defaultOptions
instance FromJSON DaemonMsg
instance ToJSON DaemonMsg where
    toEncoding = genericToEncoding defaultOptions

instance FromJSON LightIdentifier
instance ToJSON LightIdentifier where
    toEncoding = genericToEncoding defaultOptions
instance FromJSON RGBColor
instance ToJSON RGBColor where
    toEncoding = genericToEncoding defaultOptions
instance FromJSON LightPower
instance ToJSON LightPower where
    toEncoding = genericToEncoding defaultOptions
instance FromJSON StateUpdate
instance ToJSON StateUpdate where
    toEncoding = genericToEncoding defaultOptions

instance FromJSON LightData
instance ToJSON LightData where
    toEncoding = genericToEncoding defaultOptions
instance FromJSON LightColor
instance ToJSON LightColor where
    toEncoding = genericToEncoding defaultOptions
