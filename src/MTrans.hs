module MTrans where

import Interface.Cache.Class as Cache (MIOCache)
import Interface.Error.Class as Error (MIOError)
import Interface.Log.Class as Log (MLog)

class (MIOCache m, MIOError m, MLog m) => MTrans m