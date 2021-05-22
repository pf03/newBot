{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveGeneric #-}

module Transformer.Types where

import Control.Monad.State.Lazy ( MonadIO, MonadTrans(lift), StateT(..), MonadState )
import Control.Monad.Trans.Except (ExceptT, catchE, throwE)
import Interface.Class (MCache, MError, MIOCache, MIOError, MLog, MT)
import qualified Interface.MCache.Exports as Cache
import qualified Interface.MError.Exports as Error
import qualified Interface.MLog.Exports as Log
-- import Transformer.Internal as Internal
--     ( State,
--       getLogSettings,
--       setLogSettings,
--       getLogConfig,
--       getCache,
--       setCache,
--       writeCache )

import Common.Misc ( for ) 
import Control.Monad.State.Lazy ( MonadIO, when, MonadState(get), gets, modify )
import qualified Data.Aeson.Encode.Pretty as Aeson
import qualified Data.ByteString.Lazy as L
import GHC.Generics (Generic)
import Interface.Class ( MIOError, MError, MCache ) 
import qualified Interface.MError.Exports as Error
import qualified Interface.MLog.Exports as Log
import qualified Logic.Config.Exports as Config
import qualified Interface.MCache.Exports as Cache

-----------------------------Types---------------------------------------------
newtype Transformer a = Transformer { getTransformer :: StateT State (ExceptT Error.E IO) a} 
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
  catch ta f = Transformer . StateT $ \s -> catchE (runStateT (getTransformer  ta) s) $ \e -> runStateT  (getTransformer $ f e) s

instance MIOError Transformer

instance MCache Transformer where
  getCache = Transformer _getCache
  setCache c = Transformer . _setCache $ c

instance MIOCache Transformer where
  writeCache = _writeCache

instance MT Transformer

-----------------------------Internal------------------------------------------

-- We cannot replace State to Types.hs due to cyclic dependencies
-- This is the internal structure of Transformer, which is not visible for the external user
data State = State
  {
    cache :: Cache.Cache,
    configLog :: Log.Config,
    logSettings :: Log.Settings
  }
  deriving (Show, Generic)

getLogSettings :: MonadState State m => m Log.Settings
getLogSettings = gets logSettings

setLogSettings :: MonadState State m => Log.ColorScheme -> Log.Enable -> Log.FuncName -> m ()
setLogSettings cs en fn = modify $ \s -> s {logSettings = Log.Settings cs en fn}

getLogConfig :: MonadState State m => m Log.Config
getLogConfig = gets configLog

_getCache :: MonadState State m => m Cache.Cache
_getCache = gets cache

_setCache :: MonadState State m => Cache.Cache -> m ()
_setCache c = modify $ \s -> s {cache = c}

_writeCache :: (MCache m, MIOError m, MonadState State m, MonadIO m) => m ()
_writeCache = do
    ch <- Cache.getCacheChanged
    when ch $ do
      s <- get
      saveState s
      Cache.resetCacheChanged

-----------------------------State <-> Config----------------------------------
-- readStates :: MIOError m => m [State]
-- readStates = configToStates <$> Config.readConfig

saveState :: MIOError m => State -> m ()
saveState s = do
  config <- Config.readConfig
  let newConfig = fromState config s
  Error.liftEIO $ L.writeFile Config.pathConfig (Aeson.encodePretty newConfig)


configToStates :: Config.Config -> [State]
configToStates config =
  let cas = filter Cache.enable (Config.apps config)
  in for cas $ \ca ->  
    let c =
          Cache.Cache
            { Cache.configApp = ca,
              Cache.configText = Config.text config,
              Cache.changed = False,
              Cache.defaultRepeatNumber = Config.defaultRepeatNumber config
            }
    in State
        {
          cache = c,
          configLog = Config.log config,
          logSettings = Log.defaultSettings
        }

-- not updated!!!
fromState :: Config.Config -> State -> Config.Config
fromState config st = config {Config.apps = newConfigApps}
  where
    configApps = Config.apps config
    cac = cache st
    newConfigApps = [Cache.configApp cac] <> filter (\ca -> Cache.app ca /= Cache.app (Cache.configApp cac)) configApps