{-# LANGUAGE DeriveGeneric #-}
module HkHue.Messages
    ( ClientMsg(..)
    , DaemonMsg(..)
    )
where

import           Data.Aeson                     ( FromJSON
                                                , ToJSON(..)
                                                , Value
                                                , genericToEncoding
                                                , defaultOptions
                                                )
import           GHC.Generics

import qualified Data.Text                     as T

-- TODO: Expand the "Value" out so the daemon does Hue conversions instead
-- of the clients
data ClientMsg = SetColorAllLights { cRed :: Int, cGreen :: Int, cBlue :: Int }
               | SetLightState { lightId :: Int, lightState :: Value }
               | SetAllState { lightState :: Value }
               deriving (Generic, Show)


-- Daemon Messages

newtype DaemonMsg = ProtocolError T.Text
                    deriving (Generic, Show)


-- Aeson Classes

instance FromJSON ClientMsg
instance ToJSON ClientMsg where
    toEncoding = genericToEncoding defaultOptions
instance FromJSON DaemonMsg
instance ToJSON DaemonMsg where
    toEncoding = genericToEncoding defaultOptions
