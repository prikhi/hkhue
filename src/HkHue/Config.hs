{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
module HkHue.Config
    ( getConfig
    , defaultBindAddress
    , defaultBindPort
    )
where

import           Control.Exception.Safe         ( try )
import           Data.Aeson                     ( FromJSON )
import           Data.Default                   ( Default(def) )
import           Data.Yaml                      ( prettyPrintParseException )
import           Data.Yaml.Config               ( loadYamlSettings
                                                , ignoreEnv
                                                )
import           Filesystem                     ( isFile )
import           Filesystem.Path.CurrentOS      ( decodeString )
import           System.Environment.XDG.BaseDir ( getUserConfigFile )

import qualified Data.Text                     as T

getConfig :: (FromJSON a, Default a) => IO a
getConfig = do
    configFile <- getUserConfigFile "hkhue" "config.yaml"
    hasConfig  <- isFile $ decodeString configFile
    if hasConfig
        then try (loadYamlSettings [configFile] [] ignoreEnv) >>= \case
            Left err ->
                putStrLn (prettyPrintParseException err)
                    >> putStrLn "Warning: Invalid Config Format, Using Defaults"
                    >> def
            Right c -> return c
        else def

defaultBindAddress :: T.Text
defaultBindAddress = "0.0.0.0"

defaultBindPort :: Int
defaultBindPort = 9160
