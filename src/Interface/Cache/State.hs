
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}

module Interface.Cache.State where

import Interface.Cache.Class ( MCache )
import Control.Monad.State.Lazy (MonadIO, MonadState (get), gets, modify, when)
import qualified Data.Aeson.Encode.Pretty as Aeson
import qualified Data.ByteString.Lazy as L

import qualified Interface.Cache.Config.Exports as Config
import qualified Interface.Cache.Functions as Cache
import qualified Interface.Cache.Types as Cache
import qualified Interface.Error.Exports as Error
import qualified Interface.Log.Exports as Log
import Transformer.Types

writeCache :: (MCache m, MonadState BotState m, MonadIO m) => m ()
writeCache = do
  cacheChanged <- Cache.getCacheChanged
  when cacheChanged $ do
    state <- get
    saveState state
    Cache.resetCacheChanged

-----------------------------State <-> Config----------------------------------
readStates :: (MonadIO m) => m [BotState]
readStates = getStatesFromConfig <$> Config.readConfig

readState :: (MonadIO m) => m BotState
readState = getStateFromConfig <$> Config.readConfig

saveState :: (MonadIO m) => BotState -> m ()
saveState state = do
  config <- Config.readConfig
  let newConfig = setStateToConfig config state
  Error.liftEIO $ L.writeFile Config.pathConfig (Aeson.encodePretty newConfig)

getStateFromConfig :: Config.Config -> BotState
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
   in BotState
        { cache = cache0,
          configLog = Config.configLog config,
          logSettings = Log.defaultSettings
        }

getStatesFromConfig :: Config.Config -> [BotState]
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
         in BotState
              { cache = cache0,
                configLog = Config.configLog config,
                logSettings = Log.defaultSettings
              }

setStateToConfig :: Config.Config -> BotState -> Config.Config
setStateToConfig config state = config {Config.configApps = newConfigApps}
  where
    configApps0 = Config.configApps config
    cache0 = cache state
    newConfigApps =
      [Cache.cacheConfigApp cache0]
        <> filter (\configApp0 -> Config.appName configApp0 /= Config.appName (Cache.cacheConfigApp cache0)) configApps0