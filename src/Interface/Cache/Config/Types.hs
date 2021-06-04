{-# LANGUAGE DeriveGeneric #-}

module Interface.Cache.Config.Types where

import Data.Aeson ( FromJSON, ToJSON )
import GHC.Generics (Generic)
import qualified Interface.Log.Exports as Log
import Common.Types ( UpdateId, ChatId, Host, Token )
import qualified Data.Map.Internal as M

data App = Telegram | VK deriving (Show, Generic, Eq)

instance ToJSON App

instance FromJSON App

data Config = Config
  {
    configForks :: Bool,
    configName :: String,
    configDefaultRepeatNumber :: Int,
    configApps :: [ConfigApp],
    configText :: ConfigText,
    configLog :: Log.Config
  }
  deriving (Show, Generic)

instance ToJSON Config
instance FromJSON Config

-- убрать лишние префиксы и привести к camelCase для FromJSON и toJSON
-- instance FromJSON Config where
--     parseJSON = genericParseJSON defaultOptions {
--         fieldLabelModifier = drop 1 }

data ConfigApp = ConfigApp
  { appEnable :: Bool,
    appName:: String,
    app :: App,
    appHost :: Host,
    appToken :: Token,
    appUpdateId :: Maybe UpdateId,
    appRepeatNumber :: M.Map ChatId Int,
    appGroupId :: Int,
    appVersion :: String --API version
  }
  deriving (Show, Generic)

instance FromJSON ConfigApp

instance ToJSON ConfigApp

data ConfigText = ConfigText
  { textHelp :: String,
    textRepeat :: String,
    textUnknown :: String,
    textButton :: String
  }
  deriving (Show, Generic)

instance FromJSON ConfigText

instance ToJSON ConfigText