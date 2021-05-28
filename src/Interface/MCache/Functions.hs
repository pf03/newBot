module Interface.MCache.Functions where

import Common.Types (ChatId, UpdateId)
import qualified Data.Map.Internal as M
import Interface.MCache.Class (MCache (..))
import Interface.MCache.Types
    ( Cache(changed, configText, configApp, defaultRepeatNumber),
      Changed,
      ConfigApp(host, token, app, updateId, repeatNumber),
      ConfigText,
      Host,
      App )

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

getApp :: MCache m => m App
getApp = app <$> getConfigApp

setConfigApp :: MCache m => ConfigApp -> m ()
setConfigApp ca = modifyCache $ \s -> s {configApp = ca}

getConfigText :: MCache m => m ConfigText
getConfigText = getsCache configText

getmUpdateId :: MCache m => m (Maybe UpdateId)
getmUpdateId = updateId <$> getConfigApp

-- updateId and repeatNumber only can be changed
setmUpdateId :: MCache m => Maybe UpdateId -> m ()
setmUpdateId muid = do
  setCacheChanged
  ca <- getConfigApp
  setConfigApp ca {updateId = muid}

getRepeatNumbers :: MCache m => m (M.Map ChatId Int)
getRepeatNumbers = repeatNumber <$> getConfigApp

setRepeatNumbers :: MCache m => M.Map ChatId Int -> m ()
setRepeatNumbers rns = do
  ca <- getConfigApp
  setConfigApp ca {repeatNumber = rns}

getmRepeatNumber :: MCache m => ChatId -> m (Maybe Int)
getmRepeatNumber cid = M.lookup cid <$> getRepeatNumbers

getRepeatNumber :: MCache m => ChatId -> m Int
getRepeatNumber cid = do
  mrn <- getmRepeatNumber cid
  case mrn of
    Nothing -> getsCache defaultRepeatNumber
    Just n -> return n

setRepeatNumber :: MCache m => ChatId -> Int -> m ()
setRepeatNumber cid rn = do
  setCacheChanged
  rns <- getRepeatNumbers
  setRepeatNumbers $ M.insert cid rn rns