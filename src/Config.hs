--importPriority = 40
module Config --(readConfig, Config, host, token ) 
where

--наши модули
import Error --70
import Parse --50
--import Logic 
import Types --100
--import Transformer 
--import App
import Log
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
    bs <- ExceptT $ toEE (L.readFile pathConfig) `catch` hR
    fileConfig <- toE $ eDecode bs
    --print fileConfig
    return fileConfig

readS :: ExceptT E IO S
readS = do
    config <- readConfig
    let s = getS config
    --print config
    return s


-- !!!!!!!!!!!!тут нужно переделать, чтобы не считывать каждый раз конфиг заново, а запоминать его!!!!!!!!!!!!!!
saveS :: S -> ExceptT E IO ()
saveS s = do
    config <- readConfig
    let newConfig = setS config s
    ExceptT $ toEE (L.writeFile pathConfig (encode newConfig)) `catch` hW

--readConfigValue :: ExceptT E IO Value
--readConfigValue = do
--    res <-  ExceptT $ toEE (L.readFile pathConfig) `catch` hR
--    toE $ getValue res

pathConfig :: FilePath
pathConfig = "config.json"

hR :: IOException -> IO (EE L.ByteString )
hR e
    | isDoesNotExistError e = throw $ ConfigError "Файл конфигурации не найден!"
    | otherwise = throw $ ConfigError "Ошибка чтения файла конфигурации"

--эту же функцию можно использовать для сохранения других данных
--saveConfig :: ToJSON a => a -> ExceptT E IO ()
--saveConfig config = ExceptT $ toEE (L.writeFile pathConfig (encode config)) `catch` hW



hW :: IOException -> IO (EE ())
hW e
    | isDoesNotExistError e = throw $ ConfigError "Файл конфигурации не найден!"
    | otherwise = throw $ ConfigError "Ошибка записи файла конфигурации"

--host :: String
--host="api.telegram.org"
--host="api.telegram.org"

-- setUpdateId :: UpdateId -> S -> S
-- setUpdateId newUid c = let newConfigApp = (configApp c) {updateId = newUid} in
--     c {configApp = newConfigApp}

--функции для работы с состоянием. Внутрення структура состояния скрывается за геттерами и сеттерами
--getters && setters lens like

-------------------State <-> Config--------------------------------------
getS :: Config -> S
getS configFile = let 
        configApps =  _apps configFile;
        configLog = _log configFile
        configApp = head $ filter (\ca -> show (_app configFile) == name ca) configApps in 
    S {
        app = _app configFile,
        configApp = configApp,
        configText = _text configFile,
        configLog = configLog,
        logSettings = defaultLogSettings
    }

setS :: Config -> S -> Config
setS configFile config = let 
        configApps =  _apps configFile;
        newConfigApps = [configApp config] <> filter (\ca -> name ca /= name (configApp config) ) configApps in
    configFile {_apps = newConfigApps}

-------------------Other-Simple--------------------------------------
-- getConfigApp :: S -> ConfigApp
-- getConfigApp = configApp --trivial

-- setConfigApp :: ConfigApp -> S -> S
-- setConfigApp ca s = s {configApp = ca}

-- getRepeatNumbers ::  S -> M.Map ChatId Int
-- getRepeatNumbers = repeatNumber . getConfigApp

-- setRepeatNumbers ::  M.Map ChatId Int -> S -> S
-- setRepeatNumbers rns s = setConfigApp (getConfigApp s) {repeatNumber = rns} s

-- getmRepeatNumber :: ChatId -> S -> Maybe Int
-- getmRepeatNumber cid = M.lookup cid . getRepeatNumbers

-- setRepeatNumber :: ChatId -> Int -> S -> S
-- setRepeatNumber cid rn s = setRepeatNumbers (M.insert cid rn (getRepeatNumbers s)) s

-----------------------Other-State-------------------------------------------------------

getConfigApp :: State S ConfigApp
getConfigApp = gets configApp --trivial
-- getConfigAppT = toT getConfigAppT

setConfigApp :: ConfigApp -> State S ()
setConfigApp ca  = modify $ \s -> s {configApp = ca}

getConfigText :: State S ConfigText
getConfigText = gets configText --trivial

getUpdateId :: State S UpdateId
getUpdateId = updateId <$> getConfigApp

setUpdateId :: UpdateId -> State S ()
setUpdateId uid = do
    ca <- getConfigApp
    setConfigApp ca {updateId = uid}
-- setUpdateIdT = toT . setUpdateId

getRepeatNumbers ::  State S (M.Map ChatId Int)
getRepeatNumbers = repeatNumber <$> getConfigApp

setRepeatNumbers ::  M.Map ChatId Int -> State S ()
setRepeatNumbers rns = do
    ca <- getConfigApp
    setConfigApp ca {repeatNumber = rns}

getmRepeatNumber :: ChatId -> State S (Maybe Int)
getmRepeatNumber cid = M.lookup cid <$> getRepeatNumbers

setRepeatNumber :: ChatId -> Int -> State S ()
setRepeatNumber cid rn = do
    rns <- getRepeatNumbers
    setRepeatNumbers $ M.insert cid rn rns





