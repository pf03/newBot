module Interface.Cache.Class where

import Control.Monad.Trans.State.Lazy (State, get, gets, modify, put)
import Interface.Cache.Types (Cache)
import qualified Transformer.Types as State

class Monad m => MCache m where
  getCache :: m Cache
  setCache :: Cache -> m ()

instance MCache (State Cache) where
  getCache = get
  setCache = put

instance MCache State.BotStateIO where
  getCache = gets State.stateCache
  setCache cache = modify $ \state -> state {State.stateCache = cache}