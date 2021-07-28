{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies          #-}

module Messenger.Bot.VK.Types where

import Class ( IUpdate )
import qualified Messenger.Update.VK.Types as Update
import Prelude hiding (init)


-- New type wrappers in order to avoid orphan instances

data API = API APIGroup APIName deriving (Show)

data APIGroup = Groups | Messages deriving (Show)

data APIName = GetLongPollServer | Send deriving (Show)

type Version = String