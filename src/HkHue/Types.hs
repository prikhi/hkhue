{-# LANGUAGE OverloadedStrings #-}
{- | Types specific to the Hue Bridge API. -}
module HkHue.Types
    ( BridgeState(..)
    , BridgeLight(..)
    , BridgeLightState(..)
    )
where

import           Data.Aeson                     ( FromJSON(parseJSON)
                                                , (.:)
                                                , withObject
                                                )
import           Data.Scientific                ( Scientific )

import qualified Data.Text                     as T
import qualified Data.Map.Strict               as Map


-- Hue Bridge State
newtype BridgeState =
    BridgeState
        { bridgeLights :: Map.Map Int BridgeLight
        } deriving (Show)

instance FromJSON BridgeState where
    parseJSON = withObject "BridgeState" $ \o -> BridgeState <$> o .: "lights"

data BridgeLight =
    BridgeLight
        { blState :: BridgeLightState
        , blType :: T.Text
        , blName :: T.Text
        , blModelId :: T.Text
        , blVersion :: T.Text
        } deriving (Show)

instance FromJSON BridgeLight where
    parseJSON = withObject "BridgeLight" $ \o ->
        BridgeLight
            <$> (o .: "state")
            <*> (o .: "type")
            <*> (o .: "name")
            <*> (o .: "modelid")
            <*> (o .: "swversion")

data BridgeLightState =
    BridgeLightState
        { blsOn :: Bool
        , blsBrightness :: Int
        , blsHue :: Int
        , blsSat :: Int
        , blsXY :: (Rational, Rational)
        , blsCT :: Int
        , blsAlert :: T.Text
        , blsEffect :: T.Text
        , blsColorMode :: T.Text
        , blsReachable :: Bool
        } deriving (Show)

instance FromJSON BridgeLightState where
    parseJSON = withObject "BridgeLightState" $ \o ->
        BridgeLightState
            <$> (o .: "on")
            <*> (o .: "bri")
            <*> (o .: "hue")
            <*> (o .: "sat")
            <*> (   (\(x, y) ->
                        ( toRational (x :: Scientific)
                        , toRational (y :: Scientific)
                        )
                    )
                <$> (o .: "xy")
                )
            <*> (o .: "ct")
            <*> (o .: "alert")
            <*> (o .: "effect")
            <*> (o .: "colormode")
            <*> (o .: "reachable")
