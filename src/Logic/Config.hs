{-# LANGUAGE DeriveGeneric #-}

module Logic.Config where

import Control.Exception (IOException)
import Data.Aeson
  ( FromJSON (parseJSON),
    Options (fieldLabelModifier),
    ToJSON (toJSON),
    defaultOptions,
    genericParseJSON,
    genericToJSON,
  )
import qualified Data.ByteString.Lazy as L
import GHC.Generics (Generic)
import Interface.MCache (ConfigApp, ConfigText)
import Interface.MError (E (ConfigError), MIOError)
import qualified Interface.MError as Error
import Interface.MLog as Log (LogConfig)
import qualified Logic.Parse as Parse (eDecode)
import System.IO.Error (isDoesNotExistError)

-----------------------------Types---------------------------------------------
data App = Telegram | VK deriving (Show, Generic)

instance ToJSON App

instance FromJSON App

data Config = Config
  { _app :: App,
    _defaultRepeatNumber :: Int,
    _apps :: [ConfigApp],
    _text :: ConfigText,
    _log :: LogConfig
  }
  deriving (Show, Generic)

instance ToJSON Config where
  toJSON =
    genericToJSON
      defaultOptions
        { fieldLabelModifier = drop 1
        }

instance FromJSON Config where
  parseJSON =
    genericParseJSON
      defaultOptions
        { fieldLabelModifier = drop 1
        }

readConfig :: MIOError m => m Config
readConfig = do
  bs <- L.readFile pathConfig `Error.catchEIO` handler
  Parse.eDecode bs
  where
    handler :: IOException -> E
    handler e
      | isDoesNotExistError e = ConfigError "Configuration file not found!"
      | otherwise = ConfigError "Error reading configuration file"

pathConfig :: FilePath
pathConfig = "config.json"
