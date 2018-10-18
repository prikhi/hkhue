{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
module HkHue.Messages
    ( ClientMsg(..)
    , DaemonMsg(..)
    , StateUpdate(..)
    , RGBColor(..)
    )
where

import           Data.Aeson                     ( FromJSON
                                                , ToJSON(..)
                                                , genericToEncoding
                                                , defaultOptions
                                                )
import           Data.Data                      ( Data )
import           Data.Typeable                  ( Typeable )
import           GHC.Generics

import qualified Data.Text                     as T

-- TODO: Expand the "Value" out so the daemon does Hue conversions instead
-- of the clients
data ClientMsg = SetLightState { lightId :: Int, lightState :: StateUpdate }
               | SetAllState { lightState :: StateUpdate }
               deriving (Generic, Show)


-- Daemon Messages

newtype DaemonMsg = ProtocolError T.Text
                    deriving (Generic, Show)


-- Accessory Types

data RGBColor = RGBColor
              { cRed :: Int
              , cGreen :: Int
              , cBlue :: Int
              } deriving (Data, Typeable, Generic, Show, Eq)

data StateUpdate = StateUpdate
                 { suColor :: Maybe RGBColor
                 , suBrightness :: Maybe Int
                 , suColorTemperature :: Maybe Int
                 } deriving (Data, Typeable, Generic, Show, Eq)

-- Aeson Classes

instance FromJSON ClientMsg
instance ToJSON ClientMsg where
    toEncoding = genericToEncoding defaultOptions
instance FromJSON DaemonMsg
instance ToJSON DaemonMsg where
    toEncoding = genericToEncoding defaultOptions

instance FromJSON RGBColor
instance ToJSON RGBColor where
    toEncoding = genericToEncoding defaultOptions
instance FromJSON StateUpdate
instance ToJSON StateUpdate where
    toEncoding = genericToEncoding defaultOptions
