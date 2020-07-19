{-# LANGUAGE OverloadedStrings #-}
{- | Types specific to the Hue Bridge API. -}
module HkHue.Types
    ( BridgeState(..)
    , BridgeLight(..)
    , BridgeLightState(..)
    , BridgeGroup(..)
    , BridgeGroupState(..)
    )
where

import           Data.Aeson                     ( FromJSON(parseJSON)
                                                , (.:)
                                                , withObject
                                                )
import           Data.Maybe                     ( mapMaybe )
import           Data.Scientific                ( Scientific )
import           Text.Read                      ( readMaybe )

import qualified Data.Text                     as T
import qualified Data.Map.Strict               as Map


-- Hue Bridge State
data BridgeState =
    BridgeState
        { bridgeLights :: Map.Map Int BridgeLight
        , bridgeGroups :: Map.Map Int BridgeGroup
        } deriving (Show)

instance FromJSON BridgeState where
    parseJSON = withObject "BridgeState"
        $ \o -> BridgeState <$> o .: "lights" <*> o .: "groups"

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


data BridgeGroup =
    BridgeGroup
        { bgState :: BridgeGroupState
        , bgLights :: [Int]
        , bgName :: T.Text
        } deriving (Show)

instance FromJSON BridgeGroup where
    parseJSON = withObject "BridgeGroup" $ \o ->
        BridgeGroup
            <$> o
            .:  "action"
            <*> (mapMaybe readMaybe <$> o .: "lights")
            <*> o
            .:  "name"

data BridgeGroupState =
    BridgeGroupState
        { bgsOn :: Bool
        , bgsBrightness :: Int
        , bgsHue :: Int
        , bgsSat :: Int
        , bgsXY :: (Rational, Rational)
        , bgsCT :: Int
        , bgsEffect :: T.Text
        , bgsColorMode :: T.Text
        } deriving (Show)

instance FromJSON BridgeGroupState where
    parseJSON = withObject "BridgeGroupState" $ \o ->
        BridgeGroupState
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
            <*> (o .: "effect")
            <*> (o .: "colormode")
