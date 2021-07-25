module Interface.MTrans where

import Interface.Cache.Class as Cache (MIOCache)
-- import Interface.Error.Class as Error (MError)
import Interface.Log.Class as Log (MLog)
import Control.Monad.IO.Class (MonadIO)

class (MIOCache m, MonadIO m, MLog m) => MTrans m