{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
module HkHue.Config
    ( getConfig
    , defaultBindAddress
    , defaultBindPort
    , ClientConfig(..)
    )
where

import           Control.Exception.Safe         ( try )
import           Data.Aeson                     ( (.:)
                                                , FromJSON(parseJSON)
                                                , withObject
                                                )
import           Data.Default                   ( Default(def) )
import           Data.Yaml                      ( prettyPrintParseException )
import           Data.Yaml.Config               ( loadYamlSettings
                                                , ignoreEnv
                                                )
import           Filesystem                     ( isFile )
import           Filesystem.Path.CurrentOS      ( decodeString )
import           System.Environment.XDG.BaseDir ( getUserConfigFile )


{- | Load & Parse a Config From ~/.config/hkhue/config.yaml, falling back
to it's default value if the file does not exist.
-}
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

{- | Bind to all interfaces by default. -}
defaultBindAddress :: String
defaultBindAddress = "0.0.0.0"

{- | Bind to port 9160 by default. -}
defaultBindPort :: Int
defaultBindPort = 9160


data ClientConfig
    = ClientConfig
        { configDaemonAddress :: String
        , configDaemonPort :: Int
        }

instance FromJSON ClientConfig where
    parseJSON = withObject "ClientConfig"
        $ \o -> ClientConfig <$> o .: "bind-address" <*> o .: "bind-port"

instance Default ClientConfig where
    def = ClientConfig { configDaemonAddress = defaultBindAddress
                       , configDaemonPort    = defaultBindPort
                       }
