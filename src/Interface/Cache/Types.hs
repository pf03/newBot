{-# LANGUAGE DeriveGeneric #-}

module Interface.Cache.Types where

import Common.Types (ChatId, UpdateId)
import Data.Aeson (FromJSON, ToJSON)
import qualified Data.Map.Internal as M
import GHC.Generics (Generic)

data App = Telegram | VK deriving (Show, Generic, Eq)

instance ToJSON App

instance FromJSON App

data Cache = Cache
  { configApp :: ConfigApp,
    configText :: ConfigText,
    defaultRepeatNumber :: Int,
    changed :: Changed
  }
  deriving (Show, Generic)

type Changed = Bool

data ConfigApp = ConfigApp
  { enable :: Bool,
    name:: String,
    app :: App,
    host :: Host,
    token :: Token,
    updateId :: Maybe UpdateId,
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