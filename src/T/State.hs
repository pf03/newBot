{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs      #-}
module T.State where

-- Our modules
import           Interface.MCache           as Cache
import           Interface.MError           as Error
import           Interface.MT
import           Interface.MLog             as Log
import           Logic.Config               as Config

-- Other modules
import           Control.Monad.State.Lazy
import           Control.Monad.Trans.Except
import qualified Data.Map.Internal          as M
import           GHC.Generics               hiding (S)
import qualified Data.ByteString.Lazy as L


-----------------------------Types---------------------------------------------
type T = StateT S (ExceptT E IO)

-- data App = Telegram | VK deriving (Show, Generic)

data S = S {
    app :: App,
    cache       :: Cache,
    -- configApp :: ConfigApp,
    -- configText :: ConfigText,
    configLog   :: ConfigLog,
    logSettings :: LogSettings
} deriving (Show, Generic)

-----------------------------Instances-----------------------------------------
instance MLog T where
  getSettings = getLogSettings
  setSettings = setLogSettings
  getConfig = getLogConfig
--   setConfig = S.setLogConfig
--   message = Log.messageIO

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
    writeCache = undefined 
    
instance MT T

getLogSettings :: MonadState S m => m LogSettings
getLogSettings = gets logSettings

setLogSettings :: MonadState S m => LogSettings -> m ()
setLogSettings ls = modify $ \s -> s {logSettings = ls}

getLogConfig :: MonadState S m => m ConfigLog
getLogConfig = gets configLog

getCache :: MonadState S m => m Cache
getCache = gets cache

setCache :: MonadState S m => Cache -> m ()
setCache c = modify $ \s -> s {cache = c}

getApp :: MonadState S m =>  m App
getApp = gets app



