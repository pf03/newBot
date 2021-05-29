{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE InstanceSigs #-}

module Transformer.Types where
import qualified Transformer.Internal as Internal
import Control.Monad.State.Lazy ( MonadIO, MonadTrans(lift), StateT(..), MonadState )
import Control.Monad.Trans.Except (ExceptT, catchE, throwE)
import Interface.Class (MCache, MError, MIOCache, MIOError, MLog, MTrans)
import qualified Interface.MCache.Exports as Cache
import qualified Interface.MError.Exports as Error
import qualified Interface.MLog.Exports as Log

-----------------------------Types---------------------------------------------
newtype Transformer a = Transformer {getTransformer :: StateT Internal.State (ExceptT Error.Error IO) a}
  deriving newtype (Functor, Applicative, Monad, MonadFail, MonadIO, MonadState Internal.State)

-----------------------------Instances-----------------------------------------
instance MLog Transformer where
  getSettings = Transformer Internal.getLogSettings
  setSettings colorScheme logEnable funcName = Transformer $ 
    Internal.setLogSettings colorScheme logEnable funcName
  getConfig = Transformer Internal.getLogConfig
  message logConfig logSettings logLevel str = Transformer $ 
    Log.messageIO logConfig logSettings logLevel str

instance MError Transformer where
  throw :: Error.Error -> Transformer a
  throw err = Transformer . lift $ throwE err
  catch :: Transformer a -> (Error.Error -> Transformer a) -> Transformer a
  catch ta f = Transformer . StateT $ \state -> catchE (runStateT (getTransformer ta) state) $ 
    \err -> runStateT (getTransformer $ f err) state

instance MIOError Transformer

instance MCache Transformer where
  getCache = Transformer Internal.getCache
  setCache cache = Transformer . Internal.setCache $ cache

instance MIOCache Transformer where
  writeCache = Internal.writeCache

instance MTrans Transformer