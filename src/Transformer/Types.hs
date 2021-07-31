{-# LANGUAGE DeriveGeneric #-}

module Transformer.Types where

import Control.Monad.State.Lazy ( StateT ) 
import GHC.Generics (Generic)
import qualified Interface.Cache.Types as Cache
import qualified Interface.Log.Types as Log

data BotState = BotState
  { stateCache :: Cache.Cache,
    stateConfigLog :: Log.Config,
    stateLogSettings :: Log.Settings
  }
  deriving (Show, Generic)

type Transformer = StateT BotState IO