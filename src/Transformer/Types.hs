{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE InstanceSigs #-}

module Transformer.Types where

import Control.Monad.State.Lazy (MonadIO, MonadState, MonadTrans (lift), StateT (..))
import Control.Monad.Trans.Except (ExceptT, catchE, throwE)
import Interface.Class (MCache, MError, MIOCache, MIOError, MLog, MTrans)
import qualified Interface.MCache.Exports as Cache
import qualified Interface.MError.Exports as Error
import qualified Interface.MLog.Exports as Log
import qualified Transformer.Instances as Instances

-----------------------------Types---------------------------------------------
newtype Transformer a = Transformer {getTransformer :: StateT Instances.State (ExceptT Error.Error IO) a}
  deriving newtype (Functor, Applicative, Monad, MonadFail, MonadIO, MonadState Instances.State)

-----------------------------Instances-----------------------------------------
instance MLog Transformer where
  getSettings = Transformer Instances.getLogSettings
  setSettings colorScheme logEnable funcName = Transformer $ Instances.setLogSettings colorScheme logEnable funcName
  getConfig = Transformer Instances.getLogConfig
  message logConfig logSettings logLevel str = Transformer $ Log.messageIO logConfig logSettings logLevel str

instance MError Transformer where
  throw :: Error.Error -> Transformer a
  throw err = Transformer . lift $ throwE err
  catch :: Transformer a -> (Error.Error -> Transformer a) -> Transformer a
  catch ta f = Transformer . StateT $ \state -> catchE (runStateT (getTransformer ta) state) $
    \err -> runStateT (getTransformer $ f err) state

instance MIOError Transformer

instance MCache Transformer where
  getCache = Transformer Instances.getCache
  setCache cache = Transformer . Instances.setCache $ cache

instance MIOCache Transformer where
  writeCache = Instances.writeCache

instance MTrans Transformer
