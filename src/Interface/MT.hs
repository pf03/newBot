module Interface.MT where

--Our modules
import Interface.MCache as Cache
import Interface.MError as Error
import Interface.MLog as Log


class (MIOCache m, MIOError m, MLog m) => MT m