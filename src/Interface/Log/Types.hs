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
    -- | Minimal log level as an integer
    configMinLevel :: Int
  }
  deriving (Show, Generic)

instance FromJSON Config where
  parseJSON = genericParseJSON $ deletePrefixOptions 6

instance ToJSON Config where
  toJSON = genericToJSON $ deletePrefixOptions 6

data Level
  = Debug -- Debug data
  | Info -- Information about app work
  | Warn -- Warnings
  | Error -- Non-critical error, that can be given to the user in one form or another
  | Critical -- Critical error leading to application termination
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