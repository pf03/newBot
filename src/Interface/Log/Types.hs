module Interface.Log.Types where

import Data.Aeson
    ( genericParseJSON,
      genericToJSON,
      FromJSON(parseJSON),
      ToJSON(toJSON) )
import GHC.Generics (Generic)
import System.Console.ANSI (Color)
import Common.Functions ( deletePrefixOptions )

data Config = Config
  { configColorEnable :: Enable,
    configTerminalEnable :: Enable,
    configFileEnable :: Enable,
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

type FuncName = String

type ColorScheme = Color

type Enable = Bool

data Settings = Settings
  { settingsColorScheme :: ColorScheme,
    settingsEnable :: Enable,
    settingsFuncName :: String
  }
  deriving (Show)