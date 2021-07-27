{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies          #-}

module Messenger.Bot.VK.Types (Pointer (..)) where

import Class (IBot, IUpdate, MTrans)
import Common.Types (Label, UpdateId)
import qualified Messenger.Bot.Class as Class
import qualified Messenger.Bot.VK.Instances as Instances
import qualified Messenger.Update.VK.Types as Update
import Prelude hiding (init)

-----------------------------Types---------------------------------------------
data Pointer = Pointer

-- New type wrappers in order to avoid orphan instances
newtype WrapInit = WrapInit Update.Init

newtype WrapUpdate = WrapUpdate Update.Update deriving newtype (IUpdate)

-----------------------------Instance------------------------------------------
instance IBot Pointer where

  type UpdateType Pointer = WrapUpdate
  type InitType Pointer = WrapInit

  getInit :: MTrans m => Pointer -> m WrapInit
  getInit _ = WrapInit <$> Instances.getInit

  getMUpdateId :: Pointer -> WrapInit -> Maybe UpdateId
  getMUpdateId _ (WrapInit init) = Just $ Update.ts init

  getUpdates :: MTrans m => Pointer -> WrapInit -> m ([WrapUpdate], WrapInit)
  getUpdates _ (WrapInit init) = do
    (updates, newInit) <- Instances.getUpdates init
    return (WrapUpdate <$> updates, WrapInit newInit)

  sendMessage :: MTrans m => Pointer -> WrapUpdate -> [Label] -> m ()
  sendMessage _ (WrapUpdate update) = Instances.sendMessage update