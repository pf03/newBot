{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Messenger.Bot.Telegram.Types (Pointer (..)) where

import Common.Types ( Label, UpdateId )
import Interface.Class (IBot, IUpdate, MTrans)
import qualified Messenger.Bot.Class as Class
import qualified Messenger.Bot.Telegram.Instances as Instances
import qualified Messenger.Update.Telegram.Types as Update

-----------------------------Types---------------------------------------------
data Pointer = Pointer

-- New type wrappers in order to avoid orphan instances
newtype Init = Init (Maybe UpdateId)

newtype WrapUpdate = WrapUpdate Update.Update deriving newtype (IUpdate)

-----------------------------Instance------------------------------------------
instance IBot Pointer Init WrapUpdate where
  getInit :: MTrans m => Pointer -> m Init
  getInit _ = Init <$> Instances.getUpdateId

  getmUpdateId :: Init -> Maybe UpdateId
  getmUpdateId (Init mUpdateId) = mUpdateId

  getUpdates :: MTrans m => Init -> m ([WrapUpdate], Init)
  getUpdates (Init mUpdateId) = do
    (updates, newmUpdateId) <- Instances.getUpdates mUpdateId
    return (WrapUpdate <$> updates, Init newmUpdateId)
  sendMessage :: MTrans m => WrapUpdate -> [Label] -> m ()
  sendMessage (WrapUpdate update) btns = Instances.sendMessage update btns