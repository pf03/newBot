{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}

module Transformer.State where

import Class (MCache)
import Control.Monad.State.Lazy (MonadIO, MonadState (get), gets, modify, when)
import qualified Data.Aeson.Encode.Pretty as Aeson
import qualified Data.ByteString.Lazy as L
import GHC.Generics (Generic)
import qualified Interface.Cache.Config.Exports as Config
import qualified Interface.Cache.Exports as Cache
import qualified Interface.Error.Exports as Error
import qualified Interface.Log.Exports as Log

-- We cannot replace State to Types.hs due to cyclic dependencies
-- This is the internal structure of Transformer, which is not visible for the external user
data State = State
  { cache :: Cache.Cache,
    configLog :: Log.Config,
    logSettings :: Log.Settings
  }
  deriving (Show, Generic)

getLogSettings :: MonadState State m => m Log.Settings
getLogSettings = gets logSettings

setLogSettings :: MonadState State m => Log.ColorScheme -> Log.Enable -> Log.FuncName -> m ()
setLogSettings colorScheme logEnable funcName = modify $ \state ->
  state {logSettings = Log.Settings colorScheme logEnable funcName}

getLogConfig :: MonadState State m => m Log.Config
getLogConfig = gets configLog

getCache :: MonadState State m => m Cache.Cache
getCache = gets cache

setCache :: MonadState State m => Cache.Cache -> m ()
setCache cache0 = modify $ \state -> state {cache = cache0}

writeCache :: (MCache m, MonadState State m, MonadIO m) => m ()
writeCache = do
  cacheChanged <- Cache.getCacheChanged
  when cacheChanged $ do
    state <- get
    saveState state
    Cache.resetCacheChanged

-----------------------------State <-> Config----------------------------------
readStates :: (MonadIO m) => m [State]
readStates = getStatesFromConfig <$> Config.readConfig

readState :: (MonadIO m) => m State
readState = getStateFromConfig <$> Config.readConfig

saveState :: (MonadIO m) => State -> m ()
saveState state = do
  config <- Config.readConfig
  let newConfig = setStateToConfig config state
  Error.liftEIO $ L.writeFile Config.pathConfig (Aeson.encodePretty newConfig)

getStateFromConfig :: Config.Config -> State
getStateFromConfig config =
  let configApp0 =
        head $
          filter (\configApp1 -> Config.appName configApp1 == Config.configName config) (Config.configApps config)
      cache0 =
        Cache.Cache
          { Cache.cacheConfigApp = configApp0,
            Cache.cacheConfigText = Config.configText config,
            Cache.cacheChanged = False,
            Cache.cacheDefaultRepeatNumber = Config.configDefaultRepeatNumber config
          }
   in State
        { cache = cache0,
          configLog = Config.configLog config,
          logSettings = Log.defaultSettings
        }

getStatesFromConfig :: Config.Config -> [State]
getStatesFromConfig config =
  let configApps = filter Config.appEnable (Config.configApps config)
   in flip fmap configApps $ \configApp0 ->
        let cache0 =
              Cache.Cache
                { Cache.cacheConfigApp = configApp0,
                  Cache.cacheConfigText = Config.configText config,
                  Cache.cacheChanged = False,
                  Cache.cacheDefaultRepeatNumber = Config.configDefaultRepeatNumber config
                }
         in State
              { cache = cache0,
                configLog = Config.configLog config,
                logSettings = Log.defaultSettings
              }

setStateToConfig :: Config.Config -> State -> Config.Config
setStateToConfig config state = config {Config.configApps = newConfigApps}
  where
    configApps0 = Config.configApps config
    cache0 = cache state
    newConfigApps =
      [Cache.cacheConfigApp cache0]
        <> filter (\configApp0 -> Config.appName configApp0 /= Config.appName (Cache.cacheConfigApp cache0)) configApps0