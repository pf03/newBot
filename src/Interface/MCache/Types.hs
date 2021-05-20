{-# LANGUAGE DeriveGeneric #-}

module Interface.MCache.Types where

import Common.Misc (ChatId, UpdateId)
import Data.Aeson (FromJSON, ToJSON)
import qualified Data.Map.Internal as M
import GHC.Generics (Generic)

data Cache = Cache
  { configApp :: ConfigApp,
    configText :: ConfigText,
    defaultRepeatNumber :: Int,
    changed :: Changed
  }
  deriving (Show, Generic)

type Changed = Bool

data ConfigApp = ConfigApp
  { name :: String,
    host :: Host,
    token :: Token,
    updateId :: UpdateId,
    updateIdFromFile :: Bool,
    repeatNumber :: M.Map ChatId Int,
    groupId :: Int,
    version :: String --API version
  }
  deriving (Show, Generic)

instance FromJSON ConfigApp

instance ToJSON ConfigApp

data ConfigText = ConfigText
  { help :: String,
    repeat :: String,
    unknown :: String,
    button :: String
  }
  deriving (Show, Generic)

instance FromJSON ConfigText

instance ToJSON ConfigText

type Token = String

type Host = String