{-# LANGUAGE DeriveGeneric #-}
--importPriority = 40
module Logic.Config 
where

-- Our modules
import Interface.MError as Error
import Interface.MCache as Cache
import Logic.Parse as Parse 
import Interface.MLog as Log
-- import Class


import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString.Lazy.Char8 as LC
import Data.Aeson
import Control.Exception
import System.IO.Error (isDoesNotExistError)
--import GHC.Generics
import qualified Data.Map.Internal as M
import GHC.Generics

-----------------------------Types---------------------------------------------
data Config = Config {
    _app :: App,
    _apps :: [ConfigApp],
    _text :: ConfigText,
    _log :: ConfigLog
} deriving (Show, Generic)

instance ToJSON Config where
    toJSON = genericToJSON defaultOptions {
        fieldLabelModifier = drop 1 } 

instance FromJSON Config where
    parseJSON = genericParseJSON defaultOptions {
        fieldLabelModifier = drop 1 }

-----------------------------Functions-----------------------------------------

readConfig :: MIOError m => m Config
readConfig = do
    bs <- L.readFile pathConfig `Error.catchEIO` handler
    Parse.eDecode bs where
        handler :: IOException -> E
        handler e
            | isDoesNotExistError e = ConfigError "Configuration file not found!"
            | otherwise = ConfigError "Error reading configuration file"




--readConfigValue :: ExceptT E IO Value
--readConfigValue = do
--    res <-  ExceptT $ toEE (L.readFile pathConfig) `catch` hR
--    toE $ getValue res

pathConfig :: FilePath
pathConfig = "config.json"

-- hR :: IOException -> IO (Error.EE L.ByteString )
-- hR e
--     | isDoesNotExistError e = return $ Left  $ ConfigError "Файл конфигурации не найден!"
--     | otherwise = return $ Left  $ ConfigError "Ошибка чтения файла конфигурации"

-- hW :: IOException -> IO (Error.EE ())
-- hW e
--     | isDoesNotExistError e = return $ Left  $ ConfigError "Файл конфигурации не найден!"
--     | otherwise = return $ Left  $ ConfigError "Ошибка записи файла конфигурации"










