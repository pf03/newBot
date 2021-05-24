{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module VK.Bot.Types (Pointer (..)) where

import Common.Types (Label, UpdateId)
import Interface.Class (IBot, IUpdate, MTrans)
import qualified Interface.Messenger.IBot as IBot
import VK.Bot.Internal as Internal (getInit, getUpdates, sendMessage)
import qualified VK.Update as Update

-----------------------------Types---------------------------------------------
data Pointer = Pointer

-- New type wrappers in order to avoid orphan instances
newtype WrapInit = WrapInit Update.Init

newtype WrapUpdate = WrapUpdate Update.Update deriving newtype (IUpdate)

-----------------------------Instance------------------------------------------
instance IBot Pointer WrapInit WrapUpdate where
  getInit :: MTrans m => Pointer -> m WrapInit
  getInit _ = WrapInit <$> Internal.getInit

  getUpdateId :: WrapInit -> UpdateId
  getUpdateId (WrapInit ini) = Update.ts ini

  setUpdateId :: WrapInit -> UpdateId -> WrapInit
  setUpdateId (WrapInit ini) newts = WrapInit $ ini {Update.ts = newts}

  getUpdates :: MTrans m => WrapInit -> m ([WrapUpdate], WrapInit)
  getUpdates (WrapInit ini) = do
    (us, newini) <- Internal.getUpdates ini
    return (WrapUpdate <$> us, WrapInit newini)

  sendMessage :: MTrans m => WrapUpdate -> [Label] -> m ()
  sendMessage (WrapUpdate u) = Internal.sendMessage u