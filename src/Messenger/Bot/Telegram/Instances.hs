{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies          #-}

module Messenger.Bot.Telegram.Instances where

import Class (IBot, MTrans)
import Common.Types (Label, UpdateId)
import qualified Messenger.Bot.Class as Bot
import qualified Messenger.Bot.Telegram.Internal as Internal
import Messenger.Bot.Telegram.Types
import Messenger.Update.Telegram.Types

data Pointer = Pointer

instance IBot Pointer where

  type UpdateType Pointer = Update
  type InitType Pointer = Init
  type ApiType Pointer = API

  getInit :: MTrans m => Pointer -> m Init
  getInit _ = Init <$> Internal.getUpdateId

  getMUpdateId :: Pointer -> Init -> Maybe UpdateId
  getMUpdateId _ (Init mUpdateId) = mUpdateId

  getUpdates :: MTrans m => Pointer -> Init -> m ([Update], Init)
  getUpdates _ (Init mUpdateId) = do
    (updates, newMUpdateId) <- Internal.getUpdates mUpdateId
    return ( updates, Init newMUpdateId)

  sendMessage :: MTrans m => Pointer -> Update -> [Label] -> m ()
  sendMessage _ update btns = Internal.sendMessage update btns