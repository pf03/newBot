module Interface.Cache.Exports (module Class, module Functions, module Types) where

import Interface.Cache.Class as Class (MCache (..), MIOCache (..))
import Interface.Cache.Functions as Functions
  ( getAPIVersion,
    getApp,
    getCacheChanged,
    getConfigApp,
    getConfigText,
    getGroupId,
    getHost,
    getRepeatNumber,
    getRepeatNumbers,
    getToken,
    getMRepeatNumber,
    getMUpdateId,
    getsCache,
    modifyCache,
    resetCacheChanged,
    setCacheChanged,
    setConfigApp,
    setRepeatNumber,
    setRepeatNumbers,
    setMUpdateId,
  )
import Interface.Cache.Types as Types (Cache (..))