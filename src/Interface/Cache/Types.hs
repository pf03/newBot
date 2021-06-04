module Interface.Cache.Types where

import Common.Types ( Changed )
-- import GHC.Generics (Generic)
import qualified Interface.Cache.Config.Types as Config

data Cache = Cache
  { cacheConfigApp :: Config.ConfigApp,
    cacheConfigText :: Config.ConfigText,
    cacheDefaultRepeatNumber :: Int,
    cacheChanged :: Changed
  }
  deriving (Show)