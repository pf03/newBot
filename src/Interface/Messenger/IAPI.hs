module Interface.Messenger.IAPI where

-- Our modules
import Common.Misc
import Interface.MCache as Cache

class IAPI api where 
  apiName :: api -> String 
  getPath :: Token -> api -> Path