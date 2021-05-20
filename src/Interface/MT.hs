module Interface.MT where

import Interface.MCache.Class as Cache (MIOCache)
import Interface.MError as Error (MIOError)
import Interface.MLog.Class as Log (MLog)

class (MIOCache m, MIOError m, MLog m) => MT m