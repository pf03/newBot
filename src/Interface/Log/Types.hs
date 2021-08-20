module Interface.Log.Types where

import Common.Functions (deletePrefixOptions)
import Data.Aeson
  ( FromJSON (parseJSON),
    ToJSON (toJSON),
    genericParseJSON,
    genericToJSON,
  )
import GHC.Generics (Generic)

data Config = Config
  { configColorEnabled :: Bool,
    configTerminalEnabled :: Bool,
    configFileEnabled :: Bool,
    configMinLevel :: Int
  }
  deriving (Show, Generic)

instance FromJSON Config where
  parseJSON = genericParseJSON $ deletePrefixOptions 6

instance ToJSON Config where
  toJSON = genericToJSON $ deletePrefixOptions 6

data Level
  = Debug
  | Info
  | Warn
  | Error
  | Critical
  deriving (Eq, Enum, Ord, Show, Bounded)

data ColorScheme
  = BlueScheme
  | CyanScheme
  | GreenScheme
  | YellowScheme
  | BlackScheme
  deriving (Show)

data Settings = Settings
  { settingsColorScheme :: ColorScheme,
    settingsLogEnabled :: Bool,
    settingsFuncName :: String
  }
  deriving (Show)
