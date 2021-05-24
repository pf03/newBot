module Interface.Messenger.IAPI where

import Common.Types (Path)
import Interface.MCache.Types (Token)

class IAPI api where
  apiName :: api -> String
  getPath :: Token -> api -> Path