{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
module Transformer.Internal where


import Common.Misc (template)
import Control.Monad.State.Lazy ( MonadIO, when, MonadState(get), gets, modify )
import qualified Data.Aeson.Encode.Pretty as Aeson
import qualified Data.ByteString.Lazy as L
import GHC.Generics (Generic)
import Interface.Class ( MIOError, MError, MCache ) 
import qualified Interface.MError.Exports as Error
import qualified Interface.MLog.Exports as Log
import qualified Logic.Config.Exports as Config
import qualified Interface.MCache.Exports as Cache

-- We cannot replace State to Types.hs due to cyclic dependencies
-- This is the internal structure of Transformer, which is not visible for the external user
data State = State
  { app :: Config.App,
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

getCache :: MonadState State m => m Cache.Cache
getCache = gets cache

getApp :: MonadState State m => m Config.App
getApp = gets app

setCache :: MonadState State m => Cache.Cache -> m ()
setCache c = modify $ \s -> s {cache = c}

writeCache :: (MCache m, MIOError m, MonadState State m, MonadIO m) => m ()
writeCache = do
    ch <- Cache.getCacheChanged
    when ch $ do
      s <- get
      saveS s
      Cache.resetCacheChanged

-----------------------------State <-> Config----------------------------------
readS :: MIOError m => m State
readS = do
  config <- Config.readConfig
  toS config

saveS :: MIOError m => State -> m ()
saveS s = do
  config <- Config.readConfig
  let newConfig = fromS config s
  Error.liftEIO $ L.writeFile Config.pathConfig (Aeson.encodePretty newConfig)

toS :: MError m => Config.Config -> m State
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
    State
      { app = Config.app config,
        cache = cac,
        configLog = Config.log config,
        logSettings = Log.defaultSettings
      }
  where
    configApps = Config.apps config

fromS :: Config.Config -> State -> Config.Config
fromS config st = config {Config.apps = newConfigApps}
  where
    configApps = Config.apps config
    cac = cache st
    newConfigApps = [Cache.configApp cac] <> filter (\ca -> Cache.name ca /= Cache.name (Cache.configApp cac)) configApps