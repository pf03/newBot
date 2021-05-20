{-# LANGUAGE FlexibleInstances #-}

module Interface.MError.Class where

import qualified Control.Exception as E
import Control.Monad.Except (ExceptT, MonadIO (..))
import Control.Monad.Trans.Except (catchE, throwE)
import Interface.MError.Types ( E )

class MonadFail m => MError m where
  throw :: E -> m a
  catch :: m a -> (E -> m a) -> m a

class (MError m, MonadIO m) => MIOError m

instance MError (Either E) where
  throw = Left
  catch ma f = case ma of
    Left e -> f e
    Right a -> Right a

instance MError (ExceptT E IO) where
  throw = throwE
  catch = catchE

instance MIOError (ExceptT E IO)