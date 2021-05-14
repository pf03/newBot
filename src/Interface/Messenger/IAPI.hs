module Interface.Messenger.IAPI where

import Common.Misc (Path)
import Interface.MCache as Cache (Token)

class IAPI api where
  apiName :: api -> String
  getPath :: Token -> api -> Path