{-# LANGUAGE FlexibleInstances #-}

module Interface.Cache.Class where

import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Trans.State.Lazy (State, get, put)
import Interface.Cache.Types (Cache)

class Monad m => MCache m where
  getCache :: m Cache
  setCache :: Cache -> m ()

class (MCache m, MonadIO m) => MIOCache m where
  -- Write only if cache changed
  writeCache :: m ()

instance MCache (State Cache) where
  getCache = get
  setCache = put