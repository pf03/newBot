--importPriority = 40
module Config 
where

--наши модули
import qualified Interface.Error as Error
import qualified Parse --50
import Types --100
import qualified Interface.Log as Log
import Class
import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString.Lazy.Char8 as LC
import Data.Aeson
import Control.Exception
import System.IO.Error (isDoesNotExistError)
--import GHC.Generics
import qualified Data.Map.Internal as M

import Control.Monad.Trans.Except
import Control.Monad.Trans.Class
import Control.Monad.State.Lazy

readConfig :: ExceptT E IO Config
readConfig = do
    bs <- ExceptT $ Error.toEE (L.readFile pathConfig) `catch` hR
    fileConfig <- Error.toE $ Parse.eDecode bs
    --print fileConfig
    return fileConfig

readS :: ExceptT E IO S
readS = do
    config <- readConfig
    let s = toS config
    --print config
    return s


-- !!!!!!!!!!!!тут нужно переделать, чтобы не считывать каждый раз конфиг заново, а запоминать его!!!!!!!!!!!!!!
saveS :: S -> ExceptT E IO ()
saveS s = do
    config <- readConfig
    let newConfig = fromS config s
    ExceptT $ Error.toEE (L.writeFile pathConfig (encode newConfig)) `catch` hW

--readConfigValue :: ExceptT E IO Value
--readConfigValue = do
--    res <-  ExceptT $ toEE (L.readFile pathConfig) `catch` hR
--    toE $ getValue res

pathConfig :: FilePath
pathConfig = "config.json"

hR :: IOException -> IO (Error.EE L.ByteString )
hR e
    | isDoesNotExistError e = return $ Left  $ ConfigError "Файл конфигурации не найден!"
    | otherwise = return $ Left  $ ConfigError "Ошибка чтения файла конфигурации"

hW :: IOException -> IO (Error.EE ())
hW e
    | isDoesNotExistError e = return $ Left  $ ConfigError "Файл конфигурации не найден!"
    | otherwise = return $ Left  $ ConfigError "Ошибка записи файла конфигурации"



-------------------State <-> Config--------------------------------------
toS :: Config -> S
toS configFile = let 
        configApps =  _apps configFile;
        configLog = _log configFile
        configApp = head $ filter (\ca -> show (_app configFile) == name ca) configApps in 
    S {
        app = _app configFile,
        configApp = configApp,
        configText = _text configFile,
        configLog = configLog,
        logSettings = Log.defaultSettings
    }

fromS :: Config -> S -> Config
fromS configFile config = let 
        configApps =  _apps configFile;
        newConfigApps = [configApp config] <> filter (\ca -> name ca /= name (configApp config) ) configApps in
    configFile {_apps = newConfigApps}







