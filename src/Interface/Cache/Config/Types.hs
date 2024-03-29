module Interface.Cache.Config.Types where

import Common.Functions (deletePrefixOptions)
import Common.Types (ChatId, Host, Token, UpdateId)
import Data.Aeson
  ( FromJSON (parseJSON),
    ToJSON (toJSON),
    genericParseJSON,
    genericToJSON,
  )
import qualified Data.Map.Internal as M
import GHC.Generics (Generic)
import qualified Interface.Log.Types as Log

data App = Telegram | VK deriving (Show, Generic, Eq)

instance ToJSON App

instance FromJSON App

data Config = Config
  { configForks :: Bool,
    configName :: String,
    configDefaultRepeatNumber :: Int,
    configApps :: [ConfigApp],
    configText :: ConfigText,
    configLog :: Log.Config
  }
  deriving (Show, Generic)

instance FromJSON Config where
  parseJSON = genericParseJSON $ deletePrefixOptions 6

instance ToJSON Config where
  toJSON = genericToJSON $ deletePrefixOptions 6

data ConfigApp = ConfigApp
  { appEnable :: Bool,
    appName :: String,
    appApp :: App,
    appHost :: Host,
    appToken :: Token,
    appUpdateId :: Maybe UpdateId,
    appRepeatNumber :: M.Map ChatId Int,
    appGroupId :: Int,
    appVersion :: String
  }
  deriving (Show, Generic)

instance FromJSON ConfigApp where
  parseJSON = genericParseJSON $ deletePrefixOptions 3

instance ToJSON ConfigApp where
  toJSON = genericToJSON $ deletePrefixOptions 3

data ConfigText = ConfigText
  { textHelp :: String,
    textRepeat :: String,
    textUnknown :: String,
    textButton :: String
  }
  deriving (Show, Generic)

instance FromJSON ConfigText where
  parseJSON = genericParseJSON $ deletePrefixOptions 4

instance ToJSON ConfigText where
  toJSON = genericToJSON $ deletePrefixOptions 4