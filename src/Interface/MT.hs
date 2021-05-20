module Interface.MT (MLog, MError, MIOError, MCache, MIOCache, MT) where

-- Re-export all classes

import Interface.MCache ( MIOCache, MCache )
import Interface.MError ( MIOError, MError )
import Interface.MLog.Class (MLog)

class (MIOCache m, MIOError m, MLog m) => MT m