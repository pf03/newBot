{-# LANGUAGE DeriveGeneric #-}
module Interface.MCache where

-- Our modules
import Common.Misc

-- Other modules
import           GHC.Generics
import Control.Monad.IO.Class
import qualified Data.Map.Internal as M
import Data.Aeson


-----------------------------Types---------------------------------------------
data Cache = Cache {
    -- app :: App,
    configApp :: ConfigApp,
    configText :: ConfigText,
    changed :: Changed
    -- configLog :: ConfigLog,
    -- logSettings :: LogSettings
} deriving (Show, Generic)
type Changed = Bool

data ConfigApp = ConfigApp{
    name:: String,
    host :: Host,
    token :: Token,
    updateId :: UpdateId,
    updateIdFromFile :: Bool,
    repeatNumber :: M.Map ChatId Int,
    groupId :: Int,
    version :: String --API version

} deriving (Show, Generic)

instance FromJSON ConfigApp
instance ToJSON ConfigApp

data ConfigText = ConfigText{
    help :: String, 
    repeat :: String, 
    unknown :: String, 
    button :: String
} deriving (Show, Generic)
instance FromJSON ConfigText
instance ToJSON ConfigText

type Token = String
type Host = String



-----------------------------Class---------------------------------------------
class Monad m => MCache m where
    getCache :: m Cache
    setCache :: Cache -> m ()
    -- getCacheChanged :: m Changed --Changed можно сделать частью Cache и вынести эти функции в свободные
    -- setCacheChanged :: Changed -> m ()
    

class (MCache m, MonadIO m) => MIOCache m where
    -- Write only if cache changed
    -- можно сюда еще добавить обработку ошибок и логгирование, тогда это уже трансформер.
    writeCache :: m () 



getCacheChanged :: MCache m => m Changed 
getCacheChanged = getsCache changed

setCacheChanged :: MCache m => Changed -> m ()
setCacheChanged ch = modifyCache $ \s -> s {changed = ch}

getsCache :: MCache m => (Cache -> a) -> m a
getsCache f = f <$> getCache

modifyCache :: MCache m => (Cache -> Cache) -> m ()
modifyCache f = do
    cache <- getCache
    setCache $ f cache

-- data Cache = Cache {
--     app :: App,
--     configApp :: ConfigApp,
--     configText :: ConfigText,
--     configLog :: ConfigLog,
--     logSettings :: LogSettings
-- } deriving (Show, Generic)



-- getApp :: MCache m => m App
-- getApp = getsCache app

getConfigApp :: MCache m => m ConfigApp
getConfigApp = getsCache configApp --trivial

getHost :: MCache m => m Host
getHost = host <$> getConfigApp

getToken :: MCache m => m Host
getToken = token <$> getConfigApp

setConfigApp :: MCache m => ConfigApp -> m ()
setConfigApp ca  = modifyCache $ \s -> s {configApp = ca}

getConfigText :: MCache m => m ConfigText
getConfigText = getsCache configText --trivial

getUpdateId :: MCache m => m UpdateId
getUpdateId = updateId <$> getConfigApp

setUpdateId :: MCache m => UpdateId -> m ()
setUpdateId uid = do
    ca <- getConfigApp
    setConfigApp ca {updateId = uid}
-- setUpdateIdT = toT . setUpdateId

getUpdateIdFromFile :: MCache m => MCache m => m Bool 
getUpdateIdFromFile = getsCache $ updateIdFromFile . configApp

getRepeatNumbers ::  MCache m => m (M.Map ChatId Int)
getRepeatNumbers = repeatNumber <$> getConfigApp

setRepeatNumbers ::  MCache m => M.Map ChatId Int -> m ()
setRepeatNumbers rns = do
    ca <- getConfigApp
    setConfigApp ca {repeatNumber = rns}

getmRepeatNumber :: MCache m => ChatId -> m (Maybe Int)
getmRepeatNumber cid = M.lookup cid <$> getRepeatNumbers

setRepeatNumber :: MCache m => ChatId -> Int -> m ()
setRepeatNumber cid rn = do
    rns <- getRepeatNumbers
    setRepeatNumbers $ M.insert cid rn rns