{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}

module Interface.Cache.Config.State where

import Control.Monad.State.Lazy (MonadIO, MonadState (get), when)
import qualified Data.Aeson.Encode.Pretty as Aeson
import qualified Data.ByteString.Lazy as L
import qualified Interface.Cache.Config.Functions as Config
import qualified Interface.Cache.Config.Types as Config
import qualified Interface.Cache.Config.Internal as Config
import qualified Interface.Cache.Functions as Cache
import qualified Interface.Cache.Types as Cache
import qualified Interface.Error.Exports as Error
import qualified Interface.Log.Exports as Log
import Transformer.Types (BotState (..), Transformer)
import Control.Exception (throwIO)
import Control.Monad.IO.Class (MonadIO (liftIO))

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
readState = do
  config <- Config.readConfig
  either (liftIO . throwIO) return (getStateFromConfig config) 

saveState :: (MonadIO m) => BotState -> m ()
saveState state = do
  config <- Config.readConfig
  let newConfig = setStateToConfig config state
  Error.liftEIO $ L.writeFile Config.pathConfig (Aeson.encodePretty newConfig)

getStateFromConfig :: Config.Config -> Either Error.Error BotState
getStateFromConfig config =
  let eConfigApp = Config.getConfigAppByName config
   in fmap (buildState config) eConfigApp

getStatesFromConfig :: Config.Config -> [BotState]
getStatesFromConfig config =
  let configApps = filter Config.appEnable (Config.configApps config)
   in fmap (buildState config) configApps

buildState :: Config.Config -> Config.ConfigApp -> BotState
buildState config configApp =
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