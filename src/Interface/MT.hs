module Interface.MT where

import Interface.MCache as Cache (MIOCache)
import Interface.MError as Error (MIOError)
import Interface.MLog as Log (MLog)

class (MIOCache m, MIOError m, MLog m) => MT m