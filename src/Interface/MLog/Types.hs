{-# LANGUAGE DeriveGeneric #-}

module Interface.MLog.Types where

import Data.Aeson.Types (FromJSON, ToJSON)
import GHC.Generics (Generic)
import System.Console.ANSI (Color)

data LogConfig = LogConfig
  { colorEnable :: Enable,
    terminalEnable :: Enable,
    fileEnable :: Enable,
    -- | Minimal log level as an integer
    minLevel :: Int
  }
  deriving (Show, Generic)

instance FromJSON LogConfig

instance ToJSON LogConfig

data LogLevel
  = Debug -- Debug data
  | Info -- Information about app work
  | Warn -- Warnings
  | Error -- Non-critical error, that can be given to the user in one form or another
  | Critical -- Critical error leading to application termination
  deriving (Eq, Enum, Ord, Show, Bounded)

type FuncName = String

type ColorScheme = Color

type Enable = Bool

data LogSettings = LogSettings
  { colorScheme :: ColorScheme,
    enable :: Enable,
    funcName :: String
  }
  deriving (Show)