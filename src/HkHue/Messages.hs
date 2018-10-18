{-# LANGUAGE DeriveGeneric #-}
module HkHue.Messages
    ( ClientMsg(..)
    , DaemonMsg(..)
    )
where

import           Data.Aeson                     ( FromJSON
                                                , ToJSON(..)
                                                , genericToEncoding
                                                , defaultOptions
                                                )
import           GHC.Generics

import qualified Data.Text                     as T

data ClientMsg = NoOp
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
