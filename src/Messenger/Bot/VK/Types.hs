{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE MultiParamTypeClasses #-}

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
instance IBot Pointer WrapInit WrapUpdate where
  getInit :: MTrans m => Pointer -> m WrapInit
  getInit _ = WrapInit <$> Instances.getInit

  getMUpdateId :: WrapInit -> Maybe UpdateId
  getMUpdateId (WrapInit init) = Just $ Update.ts init

  getUpdates :: MTrans m => WrapInit -> m ([WrapUpdate], WrapInit)
  getUpdates (WrapInit init) = do
    (updates, newInit) <- Instances.getUpdates init
    return (WrapUpdate <$> updates, WrapInit newInit)

  sendMessage :: MTrans m => WrapUpdate -> [Label] -> m ()
  sendMessage (WrapUpdate update) = Instances.sendMessage update