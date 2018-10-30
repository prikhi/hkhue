{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
module HkHue.Messages
    ( ClientMsg(..)
    , DaemonMsg(..)
    , LightIdentifier(..)
    , RGBColor(..)
    , LightPower(..)
    , StateUpdate(..)
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
    | Alert { lightId :: LightIdentifier }
    | ScanLights
    | GetAverageColorTemp
    deriving (Generic, Show)


-- Daemon Messages

data DaemonMsg
    = ProtocolError T.Text
    | AverageColorTemp Int
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
