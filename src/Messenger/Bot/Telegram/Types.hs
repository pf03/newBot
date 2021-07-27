{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies          #-}

module Messenger.Bot.Telegram.Types (Pointer (..)) where

import Class (IBot, IUpdate, MTrans)
import Common.Types (Label, UpdateId)
import qualified Messenger.Bot.Class as Class
import qualified Messenger.Bot.Telegram.Instances as Instances
import qualified Messenger.Update.Telegram.Types as Update

-----------------------------Types---------------------------------------------
data Pointer = Pointer

-- New type wrappers in order to avoid orphan instances
newtype Init = Init (Maybe UpdateId)

newtype WrapUpdate = WrapUpdate Update.Update deriving newtype (IUpdate)

-----------------------------Instance------------------------------------------
instance IBot Pointer where

  type UpdateType Pointer = WrapUpdate
  type InitType Pointer = Init

  getInit :: MTrans m => Pointer -> m Init
  getInit _ = Init <$> Instances.getUpdateId

  getMUpdateId :: Pointer -> Init -> Maybe UpdateId
  getMUpdateId _ (Init mUpdateId) = mUpdateId

  getUpdates :: MTrans m => Pointer -> Init -> m ([WrapUpdate], Init)
  getUpdates _ (Init mUpdateId) = do
    (updates, newMUpdateId) <- Instances.getUpdates mUpdateId
    return (WrapUpdate <$> updates, Init newMUpdateId)
  sendMessage :: MTrans m => Pointer -> WrapUpdate -> [Label] -> m ()
  sendMessage _ (WrapUpdate update) btns = Instances.sendMessage update btns