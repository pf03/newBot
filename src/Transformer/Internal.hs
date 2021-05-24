{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}

module Transformer.Internal where

import Common.Misc (for)
import Control.Monad.State.Lazy (MonadIO, MonadState (get), gets, modify, when)
import qualified Data.Aeson.Encode.Pretty as Aeson
import qualified Data.ByteString.Lazy as L
import GHC.Generics (Generic)
import Interface.Class (MCache, MIOError)
import qualified Interface.MCache.Exports as Cache
import qualified Interface.MError.Exports as Error
import qualified Interface.MLog.Exports as Log
import qualified Logic.Config.Exports as Config

-----------------------------Internal------------------------------------------
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
setLogSettings cs en fn = modify $ \s -> s {logSettings = Log.Settings cs en fn}

getLogConfig :: MonadState State m => m Log.Config
getLogConfig = gets configLog

getCache :: MonadState State m => m Cache.Cache
getCache = gets cache

setCache :: MonadState State m => Cache.Cache -> m ()
setCache c = modify $ \s -> s {cache = c}

writeCache :: (MCache m, MIOError m, MonadState State m, MonadIO m) => m ()
writeCache = do
  ch <- Cache.getCacheChanged
  when ch $ do
    s <- get
    saveState s
    Cache.resetCacheChanged

-----------------------------State <-> Config----------------------------------
readStates :: MIOError m => m [State]
readStates = configToStates <$> Config.readConfig

readState :: MIOError m => m State
readState = configToState <$> Config.readConfig

saveState :: MIOError m => State -> m ()
saveState s = do
  config <- Config.readConfig
  let newConfig = fromState config s
  Error.liftEIO $ L.writeFile Config.pathConfig (Aeson.encodePretty newConfig)

configToState :: Config.Config -> State
configToState config =
  let ca = head $ filter (\ca0 -> Cache.name ca0 == Config.name config) (Config.apps config)
      c =
        Cache.Cache
          { Cache.configApp = ca,
            Cache.configText = Config.text config,
            Cache.changed = False,
            Cache.defaultRepeatNumber = Config.defaultRepeatNumber config
          }
   in State
        { cache = c,
          configLog = Config.log config,
          logSettings = Log.defaultSettings
        }

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
              { cache = c,
                configLog = Config.log config,
                logSettings = Log.defaultSettings
              }

fromState :: Config.Config -> State -> Config.Config
fromState config st = config {Config.apps = newConfigApps}
  where
    configApps = Config.apps config
    c = cache st
    newConfigApps = [Cache.configApp c] <> filter (\ca -> Cache.name ca /= Cache.name (Cache.configApp c)) configApps