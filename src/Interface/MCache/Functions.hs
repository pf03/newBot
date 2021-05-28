module Interface.MCache.Functions where

import Common.Types (ChatId, UpdateId)
import qualified Data.Map.Internal as M
import Interface.MCache.Class (MCache (..))
import Interface.MCache.Types
  ( App,
    Cache (changed, configApp, configText, defaultRepeatNumber),
    Changed,
    ConfigApp (app, groupId, host, repeatNumber, token, updateId, version),
    ConfigText,
    Host,
  )

getCacheChanged :: MCache m => m Changed
getCacheChanged = getsCache changed

setCacheChanged :: MCache m => m ()
setCacheChanged = modifyCache $ \cache -> cache {changed = True}

resetCacheChanged :: MCache m => m ()
resetCacheChanged = modifyCache $ \cache -> cache {changed = False}

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

getGroupId :: MCache m => m Int
getGroupId = groupId <$> getConfigApp

getApp :: MCache m => m App
getApp = app <$> getConfigApp

getAPIVersion :: MCache m => m String
getAPIVersion = version <$> getConfigApp

setConfigApp :: MCache m => ConfigApp -> m ()
setConfigApp ca = modifyCache $ \cache -> cache {configApp = ca}

getConfigText :: MCache m => m ConfigText
getConfigText = getsCache configText

getmUpdateId :: MCache m => m (Maybe UpdateId)
getmUpdateId = updateId <$> getConfigApp

-- updateId and repeatNumber only can be changed
setmUpdateId :: MCache m => Maybe UpdateId -> m ()
setmUpdateId mUpdateId = do
  setCacheChanged
  configApp0 <- getConfigApp
  setConfigApp configApp0 {updateId = mUpdateId}

getRepeatNumbers :: MCache m => m (M.Map ChatId Int)
getRepeatNumbers = repeatNumber <$> getConfigApp

setRepeatNumbers :: MCache m => M.Map ChatId Int -> m ()
setRepeatNumbers repeatNumbers = do
  configApp0 <- getConfigApp
  setConfigApp configApp0 {repeatNumber = repeatNumbers}

getmRepeatNumber :: MCache m => ChatId -> m (Maybe Int)
getmRepeatNumber chatId = M.lookup chatId <$> getRepeatNumbers

getRepeatNumber :: MCache m => ChatId -> m Int
getRepeatNumber chatId = do
  mrepeatNumber <- getmRepeatNumber chatId
  case mrepeatNumber of
    Nothing -> getsCache defaultRepeatNumber
    Just repeatNumber0 -> return repeatNumber0

setRepeatNumber :: MCache m => ChatId -> Int -> m ()
setRepeatNumber chatId repeatNumber0 = do
  setCacheChanged
  repeatNumbers <- getRepeatNumbers
  setRepeatNumbers $ M.insert chatId repeatNumber0 repeatNumbers