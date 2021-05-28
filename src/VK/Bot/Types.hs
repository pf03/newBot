{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module VK.Bot.Types (Pointer (..)) where

import Common.Types (Label, UpdateId)
import Interface.Class (IBot, IUpdate, MTrans)
import qualified Interface.Messenger.IBot as IBot
import qualified VK.Bot.Internal as Internal
import qualified VK.Update as Update
import Prelude hiding (init)
-----------------------------Types---------------------------------------------
data Pointer = Pointer

-- New type wrappers in order to avoid orphan instances
newtype WrapInit = WrapInit Update.Init

newtype WrapUpdate = WrapUpdate Update.Update deriving newtype (IUpdate)

-----------------------------Instance------------------------------------------
instance IBot Pointer WrapInit WrapUpdate where
  getInit :: MTrans m => Pointer -> m WrapInit
  getInit _ = WrapInit <$> Internal.getInit

  getmUpdateId :: WrapInit -> Maybe UpdateId
  getmUpdateId (WrapInit init) = Just $ Update.ts init

  getUpdates :: MTrans m => WrapInit -> m ([WrapUpdate], WrapInit)
  getUpdates (WrapInit init) = do
    (updates, newInit) <- Internal.getUpdates init
    return (WrapUpdate <$> updates, WrapInit newInit)

  sendMessage :: MTrans m => WrapUpdate -> [Label] -> m ()
  sendMessage (WrapUpdate update) = Internal.sendMessage update