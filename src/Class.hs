{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
module Class where

import Control.Monad.Trans.Except
import Control.Monad.Except
import Control.Monad.Reader
import Control.Monad.State.Lazy
import qualified Interface.Log as Log



import Network.HTTP.Simple


import Types
import qualified State as S
--import qualified Log cyclic import


----------------------------------ToTransformer--------------------------------------------
--подъем до основного трасформера
class ToTransformer m where 
    toT :: m a -> T a

instance ToTransformer T where
    toT = id

instance ToTransformer (Except E) where
    toT = lift . except . runExcept 

instance ToTransformer (ExceptT E IO) where
    toT = lift

instance ToTransformer IO where
    toT = liftIO

instance ToTransformer (Reader S) where
    toT reader = do
        s <- get
        let res = runReader reader s
        return res

instance ToTransformer (State S) where
    toT state = do
        s <- get
        let a = evalState state s
        return a

instance Log.MLog T where 
  getSettings = S.getLogSettings
  setSettings = S.setLogSettings
  --resetLogSettings = modify $ \s -> s {Log.Settings = defaultLogSettings } --Log.hs
  getConfig = S.getLogConfig



