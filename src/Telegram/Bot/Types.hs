{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Telegram.Bot.Types (Pointer (..)) where

import Common.Types ( Label, UpdateId )
import Interface.Class (IBot, IUpdate, MTrans)
import qualified Interface.Messenger.IBot as IBot
import Telegram.Bot.Internal as Internal (getUpdateId, getUpdates, sendMessage) 
import qualified Telegram.Update as Update

-----------------------------Types---------------------------------------------
data Pointer = Pointer

-- New type wrappers in order to avoid orphan instances
newtype Init = Init UpdateId

newtype WrapUpdate = WrapUpdate Update.Update deriving newtype (IUpdate)

-----------------------------Instance------------------------------------------
instance IBot Pointer Init WrapUpdate where
  getInit :: MTrans m => Pointer -> m Init
  getInit _ = Init <$> Internal.getUpdateId

  getUpdateId :: Init -> UpdateId
  getUpdateId (Init uid) = uid

  setUpdateId :: Init -> UpdateId -> Init
  setUpdateId _ newuid = Init newuid

  getUpdates :: MTrans m => Init -> m ([WrapUpdate], Init)
  getUpdates (Init uid) = do
    (us, Just newuid) <- Internal.getUpdates . Just $ uid
    return (WrapUpdate <$> us, Init newuid)

  sendMessage :: MTrans m => WrapUpdate -> [Label] -> Int -> m ()
  sendMessage (WrapUpdate u) ls = Internal.sendMessage u ls