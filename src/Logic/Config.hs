{-# LANGUAGE DeriveGeneric #-}
--importPriority = 40
module Logic.Config
where

-- Our modules
import           Interface.MCache           as Cache
import           Interface.MError           as Error
import           Interface.MLog             as Log
import           Logic.Parse                as Parse

-- Other modules
import           Control.Exception
import           Data.Aeson
import qualified Data.ByteString.Lazy       as L
import           System.IO.Error            (isDoesNotExistError)
import           GHC.Generics

-----------------------------Types---------------------------------------------
data App = Telegram | VK deriving (Show, Generic)
instance ToJSON App
instance FromJSON App

data Config = Config {
    _app  :: App,
    _apps :: [ConfigApp],
    _text :: ConfigText,
    _log  :: LogConfig
} deriving (Show, Generic)

instance ToJSON Config where
    toJSON = genericToJSON defaultOptions {
        fieldLabelModifier = drop 1 }

instance FromJSON Config where
    parseJSON = genericParseJSON defaultOptions {
        fieldLabelModifier = drop 1 }

readConfig :: MIOError m => m Config
readConfig = do
    bs <- L.readFile pathConfig `Error.catchEIO` handler
    Parse.eDecode bs where
        handler :: IOException -> E
        handler e
            | isDoesNotExistError e = ConfigError "Configuration file not found!"
            | otherwise = ConfigError "Error reading configuration file"

pathConfig :: FilePath
pathConfig = "config.json"