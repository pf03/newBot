{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveGeneric #-}

module Transformer.Types where

--import Class (MCache, MIOCache, MLog, MTrans)
import Control.Monad.State.Lazy (MonadIO, MonadState, StateT (..))
import GHC.Generics (Generic)
import qualified Interface.Cache.Types as Cache
import qualified Interface.Log.Types as Log
-- import qualified Transformer.State as State
import GHC.Generics (Generic)

-----------------------------Types---------------------------------------------

data BotState = BotState
  { cache :: Cache.Cache,
    configLog :: Log.Config,
    logSettings :: Log.Settings
  }
  deriving (Show, Generic)

type Transformer = StateT BotState IO