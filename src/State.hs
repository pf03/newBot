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

getCache :: MonadState S m => m Cache
getCache = gets cache

setCache :: MonadState S m => Cache -> m ()
setCache c = modify $ \s -> s {cache = c}
