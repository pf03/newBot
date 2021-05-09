{-# LANGUAGE DeriveGeneric #-}
module Interface.MCache where

-- Our modules
import              Common.Misc

-- Other modules
import              Control.Monad.IO.Class
import              Data.Aeson
import qualified    Data.Map.Internal as M
import              GHC.Generics

-----------------------------Types---------------------------------------------
data Cache = Cache {
    configApp :: ConfigApp,
    configText :: ConfigText,
    changed :: Changed
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
    
class (MCache m, MonadIO m) => MIOCache m where
    -- Write only if cache changed
    writeCache :: m () 

getCacheChanged :: MCache m => m Changed 
getCacheChanged = getsCache changed

setCacheChanged :: MCache m => m ()
setCacheChanged = modifyCache $ \s -> s {changed = True}

resetCacheChanged :: MCache m => m ()
resetCacheChanged = modifyCache $ \s -> s {changed = False}

getsCache :: MCache m => (Cache -> a) -> m a
getsCache f = f <$> getCache

modifyCache :: MCache m => (Cache -> Cache) -> m ()
modifyCache f = do
    cache <- getCache
    setCache $ f cache

getConfigApp :: MCache m => m ConfigApp
getConfigApp = getsCache configApp 

getHost :: MCache m => m Host
getHost = host <$> getConfigApp

getToken :: MCache m => m Host
getToken = token <$> getConfigApp

setConfigApp :: MCache m => ConfigApp -> m ()
setConfigApp ca  = modifyCache $ \s -> s {configApp = ca}

getConfigText :: MCache m => m ConfigText
getConfigText = getsCache configText

getUpdateId :: MCache m => m UpdateId
getUpdateId = updateId <$> getConfigApp

-- updateId and repeatNumber only can be changed 
setUpdateId :: MCache m => UpdateId -> m ()
setUpdateId uid = do
    setCacheChanged
    ca <- getConfigApp
    setConfigApp ca {updateId = uid}

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
    setCacheChanged
    rns <- getRepeatNumbers
    setRepeatNumbers $ M.insert cid rn rns