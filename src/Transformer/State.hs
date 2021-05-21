{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs #-}

module Transformer.State where

import Common.Misc (template)
import Control.Monad.State.Lazy (MonadState (get), MonadTrans (lift), StateT (..), gets, modify, when)
import Control.Monad.Trans.Except (ExceptT, catchE, throwE)
import qualified Data.Aeson.Encode.Pretty as Aeson
import qualified Data.ByteString.Lazy as L
import GHC.Generics (Generic)
import Interface.Class (MCache, MError, MIOCache, MIOError, MT)
import qualified Interface.MCache.Exports as Cache
import qualified Interface.MError.Exports as Error
import qualified Interface.MLog.Exports as Log
import qualified Logic.Config.Exports as Config

-----------------------------Types---------------------------------------------
type Transformer = StateT S (ExceptT Error.E IO)

data S = S
  { app :: Config.App,
    cache :: Cache.Cache,
    configLog :: Log.Config,
    logSettings :: Log.Settings
  }
  deriving (Show, Generic)

-----------------------------Instances-----------------------------------------
instance Log.MLog Transformer where
  getSettings = getLogSettings
  setSettings = setLogSettings
  getConfig = getLogConfig
  message = Log.messageIO

instance MError Transformer where
  throw :: Error.E -> Transformer a
  throw e = lift $ throwE e
  catch :: Transformer a -> (Error.E -> Transformer a) -> Transformer a
  catch ta f = StateT $ \s -> catchE (runStateT ta s) $ \e -> runStateT (f e) s

instance MIOError Transformer

instance MCache Transformer where
  getCache = gets cache
  setCache c = modify (\st -> st {cache = c})

instance MIOCache Transformer where
  writeCache = do
    ch <- Cache.getCacheChanged
    when ch $ do
      s <- get
      saveS s
      Cache.resetCacheChanged

instance MT Transformer

getLogSettings :: MonadState S m => m Log.Settings
getLogSettings = gets logSettings

setLogSettings :: MonadState S m => Log.ColorScheme -> Log.Enable -> Log.FuncName -> m ()
setLogSettings cs en fn = modify $ \s -> s {logSettings = Log.Settings cs en fn}

getLogConfig :: MonadState S m => m Log.Config
getLogConfig = gets configLog

getCache :: MonadState S m => m Cache.Cache
getCache = gets cache

setCache :: MonadState S m => Cache.Cache -> m ()
setCache c = modify $ \s -> s {cache = c}

getApp :: MonadState S m => m Config.App
getApp = gets app

-----------------------------State <-> Config----------------------------------
readS :: MIOError m => m S
readS = do
  config <- Config.readConfig
  toS config

saveS :: MIOError m => S -> m ()
saveS s = do
  config <- Config.readConfig
  let newConfig = fromS config s
  Error.liftEIO $ L.writeFile Config.pathConfig (Aeson.encodePretty newConfig)

toS :: MError m => Config.Config -> m S
toS config = do
  ca <- case filter (\ca0 -> show (Config.app config) == Cache.name ca0) configApps of
    [] -> Error.throw $ Error.ConfigError $ template "There is no app with name {0} in config" [show (Config.app config)]
    cas -> return $ head cas
  let cac =
        Cache.Cache
          { Cache.configApp = ca,
            Cache.configText = Config.text config,
            Cache.changed = False,
            Cache.defaultRepeatNumber = Config.defaultRepeatNumber config
          }
  return $
    S
      { app = Config.app config,
        cache = cac,
        configLog = Config.log config,
        logSettings = Log.defaultSettings
      }
  where
    configApps = Config.apps config

fromS :: Config.Config -> S -> Config.Config
fromS config st = config {Config.apps = newConfigApps}
  where
    configApps = Config.apps config
    cac = cache st
    newConfigApps = [Cache.configApp cac] <> filter (\ca -> Cache.name ca /= Cache.name (Cache.configApp cac)) configApps
