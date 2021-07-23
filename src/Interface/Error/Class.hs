{-# LANGUAGE FlexibleInstances #-}

module Interface.Error.Class where

import Control.Monad.Except (ExceptT)
import Control.Monad.Trans.Except (catchE, throwE)
import Interface.Error.Types (Error)

class MonadFail m => MError m where
  throw :: Error -> m a
  catch :: m a -> (Error -> m a) -> m a


instance MError (Either Error) where
  throw = Left
  catch ma f = case ma of
    Left err -> f err
    Right a -> Right a

instance MError (ExceptT Error IO) where
  throw = throwE
  catch = catchE