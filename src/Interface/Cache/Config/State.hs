{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}

module Interface.Cache.Config.State where

import Control.Monad.State.Lazy (MonadIO, MonadState (get), when)
import qualified Data.Aeson.Encode.Pretty as Aeson
import qualified Data.ByteString.Lazy as L
import qualified Interface.Cache.Config.Functions as Config
import qualified Interface.Cache.Config.Types as Config
import qualified Interface.Cache.Functions as Cache
import qualified Interface.Cache.Types as Cache
import qualified Interface.Error.Exports as Error
import qualified Interface.Log.Exports as Log
import Transformer.Types ( BotState(..), Transformer )

writeCacheToConfigFile :: Transformer ()
writeCacheToConfigFile = do
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
  let configApp =
        head $
          filter (\configApp1 -> Config.appName configApp1 == Config.configName config) (Config.configApps config)
      cache =
        Cache.Cache
          { Cache.cacheConfigApp = configApp,
            Cache.cacheConfigText = Config.configText config,
            Cache.cacheChanged = False,
            Cache.cacheDefaultRepeatNumber = Config.configDefaultRepeatNumber config
          }
   in BotState
        { stateCache = cache,
          stateConfigLog = Config.configLog config,
          stateLogSettings = Log.defaultSettings
        }

getStatesFromConfig :: Config.Config -> [BotState]
getStatesFromConfig config =
  let configApps = filter Config.appEnable (Config.configApps config)
   in flip fmap configApps $ \configApp ->
        let cache =
              Cache.Cache
                { Cache.cacheConfigApp = configApp,
                  Cache.cacheConfigText = Config.configText config,
                  Cache.cacheChanged = False,
                  Cache.cacheDefaultRepeatNumber = Config.configDefaultRepeatNumber config
                }
         in BotState
              { stateCache = cache,
                stateConfigLog = Config.configLog config,
                stateLogSettings = Log.defaultSettings
              }

setStateToConfig :: Config.Config -> BotState -> Config.Config
setStateToConfig config state = config {Config.configApps = newConfigApps}
  where
    configApps = Config.configApps config
    cache = stateCache state
    newConfigApps =
      [Cache.cacheConfigApp cache]
        <> filter (\configApp -> Config.appName configApp /= Config.appName (Cache.cacheConfigApp cache)) configApps