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
  setSettings cs e fn = Transformer $ Internal.setLogSettings cs e fn
  getConfig = Transformer Internal.getLogConfig
  message c s l st = Transformer $ Log.messageIO c s l st

instance MError Transformer where
  throw :: Error.Error -> Transformer a
  throw e = Transformer . lift $ throwE e
  catch :: Transformer a -> (Error.Error -> Transformer a) -> Transformer a
  catch ta f = Transformer . StateT $ \s -> catchE (runStateT (getTransformer ta) s) $ 
    \e -> runStateT (getTransformer $ f e) s

instance MIOError Transformer

instance MCache Transformer where
  getCache = Transformer Internal.getCache
  setCache c = Transformer . Internal.setCache $ c

instance MIOCache Transformer where
  writeCache = Internal.writeCache

instance MTrans Transformer