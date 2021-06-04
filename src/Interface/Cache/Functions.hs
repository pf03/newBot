module Interface.Cache.Functions where

import Common.Types ( UpdateId, ChatId, Host, Changed )
import qualified Data.Map.Internal as M
import Interface.Cache.Class (MCache (..))
import Interface.Cache.Types ( Cache(..) )
import qualified Interface.Cache.Config.Types as Config

getCacheChanged :: MCache m => m Changed
getCacheChanged = getsCache cacheChanged

setCacheChanged :: MCache m => m ()
setCacheChanged = modifyCache $ \cache -> cache {cacheChanged = True}

resetCacheChanged :: MCache m => m ()
resetCacheChanged = modifyCache $ \cache -> cache {cacheChanged = False}

getsCache :: MCache m => (Cache -> a) -> m a
getsCache f = f <$> getCache

modifyCache :: MCache m => (Cache -> Cache) -> m ()
modifyCache f = do
  cache <- getCache
  setCache $ f cache

getConfigApp :: MCache m => m Config.ConfigApp
getConfigApp = getsCache cacheConfigApp

getHost :: MCache m => m Host
getHost = Config.appHost <$> getConfigApp

getToken :: MCache m => m Host
getToken = Config.appToken <$> getConfigApp

getGroupId :: MCache m => m Int
getGroupId = Config.appGroupId <$> getConfigApp

getApp :: MCache m => m Config.App
getApp = Config.app <$> getConfigApp

getAPIVersion :: MCache m => m String
getAPIVersion = Config.appVersion <$> getConfigApp

setConfigApp :: MCache m => Config.ConfigApp -> m ()
setConfigApp configApp = modifyCache $ \cache -> cache {cacheConfigApp = configApp}

getConfigText :: MCache m => m Config.ConfigText
getConfigText = getsCache cacheConfigText

getmUpdateId :: MCache m => m (Maybe UpdateId)
getmUpdateId = Config.appUpdateId <$> getConfigApp

-- updateId and repeatNumber only can be changed
setmUpdateId :: MCache m => Maybe UpdateId -> m ()
setmUpdateId mUpdateId = do
  setCacheChanged
  configApp <- getConfigApp
  setConfigApp configApp {Config.appUpdateId = mUpdateId}

getRepeatNumbers :: MCache m => m (M.Map ChatId Int)
getRepeatNumbers = Config.appRepeatNumber <$> getConfigApp

setRepeatNumbers :: MCache m => M.Map ChatId Int -> m ()
setRepeatNumbers repeatNumbers = do
  configApp <- getConfigApp
  setConfigApp configApp {Config.appRepeatNumber = repeatNumbers}

getmRepeatNumber :: MCache m => ChatId -> m (Maybe Int)
getmRepeatNumber chatId = M.lookup chatId <$> getRepeatNumbers

getRepeatNumber :: MCache m => ChatId -> m Int
getRepeatNumber chatId = do
  mrepeatNumber <- getmRepeatNumber chatId
  case mrepeatNumber of
    Nothing -> getsCache cacheDefaultRepeatNumber
    Just repeatNumber0 -> return repeatNumber0

setRepeatNumber :: MCache m => ChatId -> Int -> m ()
setRepeatNumber chatId repeatNumber0 = do
  setCacheChanged
  repeatNumbers <- getRepeatNumbers
  setRepeatNumbers $ M.insert chatId repeatNumber0 repeatNumbers