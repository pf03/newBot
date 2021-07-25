{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Transformer.Types where

import Class (MCache, MIOCache, MLog, MTrans)
import Control.Monad.State.Lazy (MonadIO, MonadState, StateT (..))
import qualified Interface.Cache.Exports as Cache
import qualified Interface.Log.Exports as Log
import qualified Transformer.State as State

-----------------------------Types---------------------------------------------
newtype Transformer a = Transformer {getTransformer :: StateT State.State IO a}
  deriving newtype (Functor, Applicative, Monad, MonadFail, MonadIO, MonadState State.State)

-----------------------------Instances-----------------------------------------
instance MLog Transformer where
  getSettings = Transformer State.getLogSettings
  setSettings colorScheme logEnable funcName = Transformer $ State.setLogSettings colorScheme logEnable funcName
  getConfig = Transformer State.getLogConfig
  writeMessage logConfig logSettings logLevel str = Transformer $ Log.writeMessageIO logConfig logSettings logLevel str

instance MCache Transformer where
  getCache = Transformer State.getCache
  setCache cache = Transformer . State.setCache $ cache

instance MIOCache Transformer where
  writeCache = State.writeCache

instance MTrans Transformer
