{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs      #-}
module T.State where

-- Our modules
import           Interface.MCache           as Cache
import           Interface.MError           as Error
import           Interface.MLog             as Log
import           Interface.MT
import           Logic.Config               as Config

-- Other modules
import           Control.Monad.State.Lazy
import           Control.Monad.Trans.Except
import qualified Data.Aeson.Encode.Pretty   as Aeson
import qualified Data.ByteString.Lazy       as L
import           GHC.Generics               hiding (S)

-----------------------------Types---------------------------------------------
type T = StateT S (ExceptT E IO)

data S = S {
    app         :: App,
    cache       :: Cache,
    configLog   :: LogConfig,
    logSettings :: LogSettings
    -- exitFlag    :: Bool
} deriving (Show, Generic)

-----------------------------Instances-----------------------------------------
instance MLog T where
  getSettings = getLogSettings
  setSettings = setLogSettings
  getConfig = getLogConfig
  message = Log.messageIO

instance MError T where
    throw :: E -> T a
    throw e  = lift $ throwE e
    catch :: T a -> (E -> T a) -> T a
    catch ta f  = StateT $ \s -> catchE (runStateT ta s) $ \e -> runStateT (f e) s

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

getApp :: MonadState S m =>  m App
getApp = gets app

-----------------------------State <-> Config----------------------------------
readS :: MIOError m => m S
readS = do
    config <- readConfig
    let s = toS config
    return s

saveS :: MIOError m => S -> m ()                                                 
saveS s = do
    config <- Config.readConfig
    let newConfig = fromS config s
    liftIO $ L.writeFile pathConfig (Aeson.encodePretty newConfig)

toS :: Config -> S
toS config = S {
        app = _app config,
        cache = cac,
        configLog = _log config,
        logSettings = Log.defaultSettings
    } where
        configApps =  _apps config;
        ca = head $ filter (\ca0 -> show (_app config) == name ca0) configApps
        cac = Cache{
            configApp = ca,
            configText = _text config,
            changed = False
        }

fromS :: Config -> S -> Config
fromS config st = config {_apps = newConfigApps} where
    configApps =  _apps config;
    cac = cache st;
    newConfigApps = [configApp cac] <> filter (\ca -> name ca /= name (configApp cac) ) configApps