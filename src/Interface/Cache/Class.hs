{-# LANGUAGE FlexibleInstances #-}

module Interface.Cache.Class where

import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Trans.State.Lazy (State, get, gets, modify, put)
import Interface.Cache.Types (Cache)
import qualified Transformer.Types as State

-- import qualified Interface.Cache.State as State

class Monad m => MCache m where
  getCache :: m Cache
  setCache :: Cache -> m ()

-- class (MCache m, MonadIO m) => MIOCache m where
--   -- Write only if cache changed
--   writeCache :: m ()

instance MCache (State Cache) where
  getCache = get
  setCache = put

instance MCache State.Transformer where
  getCache = gets State.cache
  setCache cache = modify $ \state -> state {State.cache = cache}

-- instance MIOCache State.Transformer where
--   writeCache = State.writeCache