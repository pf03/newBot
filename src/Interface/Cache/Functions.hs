module Interface.Cache.Functions where

import Common.Types (ChatId, Host, Token, UpdateId)
import qualified Data.Map.Internal as M
import Interface.Cache.Class (MCache (..))
import qualified Interface.Cache.Config.Types as Config
import Interface.Cache.Types (Cache (..))

getCacheChanged :: MCache m => m Bool
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

getToken :: MCache m => m Token
getToken = Config.appToken <$> getConfigApp

getGroupId :: MCache m => m Int
getGroupId = Config.appGroupId <$> getConfigApp

getApp :: MCache m => m Config.App
getApp = Config.appApp <$> getConfigApp

getAPIVersion :: MCache m => m String
getAPIVersion = Config.appVersion <$> getConfigApp

setConfigApp :: MCache m => Config.ConfigApp -> m ()
setConfigApp configApp = modifyCache $ \cache -> cache {cacheConfigApp = configApp}

getConfigText :: MCache m => m Config.ConfigText
getConfigText = getsCache cacheConfigText

getMUpdateId :: MCache m => m (Maybe UpdateId)
getMUpdateId = Config.appUpdateId <$> getConfigApp

-- updateId and repeatNumber only can be changed
setMUpdateId :: MCache m => Maybe UpdateId -> m ()
setMUpdateId mUpdateId = do
  setCacheChanged
  configApp <- getConfigApp
  setConfigApp configApp {Config.appUpdateId = mUpdateId}

getRepeatNumbers :: MCache m => m (M.Map ChatId Int)
getRepeatNumbers = Config.appRepeatNumber <$> getConfigApp

setRepeatNumbers :: MCache m => M.Map ChatId Int -> m ()
setRepeatNumbers repeatNumbers = do
  configApp <- getConfigApp
  setConfigApp configApp {Config.appRepeatNumber = repeatNumbers}

getMRepeatNumber :: MCache m => ChatId -> m (Maybe Int)
getMRepeatNumber chatId = M.lookup chatId <$> getRepeatNumbers

getRepeatNumber :: MCache m => ChatId -> m Int
getRepeatNumber chatId = do
  mRepeatNumber <- getMRepeatNumber chatId
  case mRepeatNumber of
    Nothing -> getsCache cacheDefaultRepeatNumber
    Just repeatNumber0 -> return repeatNumber0

setRepeatNumber :: MCache m => ChatId -> Int -> m ()
setRepeatNumber chatId repeatNumber0 = do
  setCacheChanged
  repeatNumbers <- getRepeatNumbers
  setRepeatNumbers $ M.insert chatId repeatNumber0 repeatNumbers