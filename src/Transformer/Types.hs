{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE InstanceSigs #-}

module Transformer.Types where

import Class (MCache, MError, MIOCache, MError, MLog, MTrans)
import Control.Monad.State.Lazy (MonadIO, MonadState, MonadTrans (lift), StateT (..))
import Control.Monad.Trans.Except (ExceptT, catchE, throwE)
import qualified Interface.Cache.Exports as Cache
import qualified Interface.Error.Exports as Error
import qualified Interface.Log.Exports as Log
import qualified Transformer.State as State

-----------------------------Types---------------------------------------------
newtype Transformer a = Transformer {getTransformer :: StateT State.State (ExceptT Error.Error IO) a}
  deriving newtype (Functor, Applicative, Monad, MonadFail, MonadIO, MonadState State.State)

-----------------------------Instances-----------------------------------------
instance MLog Transformer where
  getSettings = Transformer State.getLogSettings
  setSettings colorScheme logEnable funcName = Transformer $ State.setLogSettings colorScheme logEnable funcName
  getConfig = Transformer State.getLogConfig
  writeMessage logConfig logSettings logLevel str = Transformer $ Log.writeMessageIO logConfig logSettings logLevel str

instance MError Transformer where
  throw :: Error.Error -> Transformer a
  throw err = Transformer . lift $ throwE err
  catch :: Transformer a -> (Error.Error -> Transformer a) -> Transformer a
  catch ta f = Transformer . StateT $ \state -> catchE (runStateT (getTransformer ta) state) $
    \err -> runStateT (getTransformer $ f err) state

instance MCache Transformer where
  getCache = Transformer State.getCache
  setCache cache = Transformer . State.setCache $ cache

instance MIOCache Transformer where
  writeCache = State.writeCache

instance MTrans Transformer
