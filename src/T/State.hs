{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs #-}

module T.State where

import Common.Misc (template)
import Control.Monad.State.Lazy (MonadState (get), MonadTrans (lift), StateT (..), gets, modify, when)
import Control.Monad.Trans.Except (ExceptT, catchE, throwE)
import qualified Data.Aeson.Encode.Pretty as Aeson
import qualified Data.ByteString.Lazy as L
import GHC.Generics (Generic)
import Interface.MCache (Cache (..), MCache, MIOCache)
import qualified Interface.MCache as Cache
import Interface.MError ( E (ConfigError),MError, MIOError)
import qualified Interface.MError as Error
import Interface.MLog.Types (ColorScheme, Enable, FuncName, LogConfig, LogSettings (LogSettings))
import Interface.MLog.Class (MLog (..))
import qualified Interface.MLog.Functions as Log
import Interface.MT (MT)
import Logic.Config (App, Config (..))
import qualified Logic.Config as Config

-----------------------------Types---------------------------------------------
type T = StateT S (ExceptT E IO)

data S = S
  { app :: App,
    cache :: Cache,
    configLog :: LogConfig,
    logSettings :: LogSettings
  }
  deriving (Show, Generic)

-----------------------------Instances-----------------------------------------
instance MLog T where
  getSettings = getLogSettings
  setSettings = setLogSettings
  getConfig = getLogConfig
  message = Log.messageIO

instance MError T where
  throw :: E -> T a
  throw e = lift $ throwE e
  catch :: T a -> (E -> T a) -> T a
  catch ta f = StateT $ \s -> catchE (runStateT ta s) $ \e -> runStateT (f e) s

instance MIOError T

instance MCache T where
  getCache = gets cache
  setCache c = modify (\st -> st {cache = c})

instance MIOCache T where
  writeCache = do
    ch <- Cache.getCacheChanged
    when ch $ do
      s <- get
      saveS s
      Cache.resetCacheChanged

instance MT T

getLogSettings :: MonadState S m => m LogSettings
getLogSettings = gets logSettings

setLogSettings :: MonadState S m => ColorScheme -> Enable -> FuncName -> m ()
setLogSettings cs en fn = modify $ \s -> s {logSettings = LogSettings cs en fn}

getLogConfig :: MonadState S m => m LogConfig
getLogConfig = gets configLog

getCache :: MonadState S m => m Cache
getCache = gets cache

setCache :: MonadState S m => Cache -> m ()
setCache c = modify $ \s -> s {cache = c}

getApp :: MonadState S m => m App
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

toS :: MError m => Config -> m S
toS config = do
  ca <- case filter (\ca0 -> show (_app config) == Cache.name ca0) configApps of
    [] -> Error.throw $ ConfigError $ template "There is no app with name {0} in config" [show (_app config)]
    cas -> return $ head cas
  let cac =
        Cache
          { configApp = ca,
            configText = _text config,
            changed = False,
            defaultRepeatNumber = _defaultRepeatNumber config
          }
  return $
    S
      { app = _app config,
        cache = cac,
        configLog = _log config,
        logSettings = Log.defaultSettings
      }
  where
    configApps = _apps config

fromS :: Config -> S -> Config
fromS config st = config {_apps = newConfigApps}
  where
    configApps = _apps config
    cac = cache st
    newConfigApps = [configApp cac] <> filter (\ca -> Cache.name ca /= Cache.name (configApp cac)) configApps
