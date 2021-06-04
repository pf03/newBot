module Messenger.API.Class where

import Common.Types (Path)
import Interface.Cache.Types (Token)

class IAPI api where
  apiName :: api -> String
  getPath :: Token -> api -> Path