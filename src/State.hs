{-# LANGUAGE FlexibleContexts #-}
module State where
import Types 
import qualified Data.Map.Internal as M

import Control.Monad.State.Lazy

--getters && setters lens like


getLogSettings :: MonadState S m => m LogSettings
getLogSettings = gets logSettings

setLogSettings :: MonadState S m => LogSettings -> m ()
setLogSettings ls = modify $ \s -> s {logSettings = ls}

getLogConfig :: MonadState S m => m ConfigLog
getLogConfig = gets configLog

getApp :: MonadState S m => m App
getApp = gets app

getConfigApp :: MonadState S m => m ConfigApp
getConfigApp = gets configApp --trivial

setConfigApp :: MonadState S m => ConfigApp -> m ()
setConfigApp ca  = modify $ \s -> s {configApp = ca}

getConfigText :: MonadState S m => m ConfigText
getConfigText = gets configText --trivial

getUpdateId :: MonadState S m => m UpdateId
getUpdateId = updateId <$> getConfigApp

setUpdateId :: MonadState S m => UpdateId -> m ()
setUpdateId uid = do
    ca <- getConfigApp
    setConfigApp ca {updateId = uid}
-- setUpdateIdT = toT . setUpdateId

getUpdateIdFromFile :: MonadState S m => MonadState S m => m Bool 
getUpdateIdFromFile = gets $ updateIdFromFile . configApp

getRepeatNumbers ::  MonadState S m => m (M.Map ChatId Int)
getRepeatNumbers = repeatNumber <$> getConfigApp

setRepeatNumbers ::  MonadState S m => M.Map ChatId Int -> m ()
setRepeatNumbers rns = do
    ca <- getConfigApp
    setConfigApp ca {repeatNumber = rns}

getmRepeatNumber :: MonadState S m => ChatId -> m (Maybe Int)
getmRepeatNumber cid = M.lookup cid <$> getRepeatNumbers

setRepeatNumber :: MonadState S m => ChatId -> Int -> m ()
setRepeatNumber cid rn = do
    rns <- getRepeatNumbers
    setRepeatNumbers $ M.insert cid rn rns
