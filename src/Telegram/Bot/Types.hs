{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Telegram.Bot.Types (Pointer (..)) where

import Common.Types ( Label, UpdateId )
import Interface.Class (IBot, IUpdate, MTrans)
import qualified Interface.Messenger.IBot as IBot
import qualified Telegram.Bot.Internal as Internal
import qualified Telegram.Update as Update

-----------------------------Types---------------------------------------------
data Pointer = Pointer

-- New type wrappers in order to avoid orphan instances
newtype Init = Init (Maybe UpdateId)

newtype WrapUpdate = WrapUpdate Update.Update deriving newtype (IUpdate)

-----------------------------Instance------------------------------------------
instance IBot Pointer Init WrapUpdate where
  getInit :: MTrans m => Pointer -> m Init
  getInit _ = Init <$> Internal.getUpdateId

  getmUpdateId :: Init -> Maybe UpdateId
  getmUpdateId (Init mUpdateId) = mUpdateId

  getUpdates :: MTrans m => Init -> m ([WrapUpdate], Init)
  getUpdates (Init mUpdateId) = do
    (updates, newmUpdateId) <- Internal.getUpdates mUpdateId
    return (WrapUpdate <$> updates, Init newmUpdateId)
  sendMessage :: MTrans m => WrapUpdate -> [Label] -> m ()
  sendMessage (WrapUpdate update) btns = Internal.sendMessage update btns