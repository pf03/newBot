{-# LANGUAGE DeriveGeneric #-}

module Logic.Config.Types where

import Data.Aeson ( FromJSON, ToJSON )
import GHC.Generics (Generic)
import qualified Interface.MCache.Exports as Cache
import qualified Interface.MLog.Exports as Log

data App = Telegram | VK deriving (Show, Generic)

instance ToJSON App

instance FromJSON App

data Config = Config
  { app :: App,
    defaultRepeatNumber :: Int,
    apps :: [Cache.ConfigApp],
    text :: Cache.ConfigText,
    log :: Log.Config
  }
  deriving (Show, Generic)

instance ToJSON Config
instance FromJSON Config