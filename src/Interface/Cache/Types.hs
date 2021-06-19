module Interface.Cache.Types where

import qualified Interface.Cache.Config.Types as Config

data Cache = Cache
  { cacheConfigApp :: Config.ConfigApp,
    cacheConfigText :: Config.ConfigText,
    cacheDefaultRepeatNumber :: Int,
    cacheChanged :: Bool
  }
  deriving (Show)