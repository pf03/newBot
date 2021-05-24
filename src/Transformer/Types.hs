{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE InstanceSigs #-}

module Transformer.Types where
import Transformer.Internal
    ( State,
      getLogSettings,
      setLogSettings,
      getLogConfig,
      _getCache,
      _setCache,
      _writeCache )
import Control.Monad.State.Lazy ( MonadIO, MonadTrans(lift), StateT(..), MonadState )
import Control.Monad.Trans.Except (ExceptT, catchE, throwE)
import Interface.Class (MCache, MError, MIOCache, MIOError, MLog, MT)
import qualified Interface.MCache.Exports as Cache
import qualified Interface.MError.Exports as Error
import qualified Interface.MLog.Exports as Log

-----------------------------Types---------------------------------------------
newtype Transformer a = Transformer {getTransformer :: StateT State (ExceptT Error.E IO) a}
  deriving newtype (Functor, Applicative, Monad, MonadFail, MonadIO, MonadState State)

-----------------------------Instances-----------------------------------------
instance MLog Transformer where
  getSettings = Transformer getLogSettings
  setSettings cs e fn = Transformer $ setLogSettings cs e fn
  getConfig = Transformer getLogConfig
  message c s l st = Transformer $ Log.messageIO c s l st

instance MError Transformer where
  throw :: Error.E -> Transformer a
  throw e = Transformer . lift $ throwE e
  catch :: Transformer a -> (Error.E -> Transformer a) -> Transformer a
  catch ta f = Transformer . StateT $ \s -> catchE (runStateT (getTransformer ta) s) $ 
    \e -> runStateT (getTransformer $ f e) s

instance MIOError Transformer

instance MCache Transformer where
  getCache = Transformer _getCache
  setCache c = Transformer . _setCache $ c

instance MIOCache Transformer where
  writeCache = _writeCache

instance MT Transformer