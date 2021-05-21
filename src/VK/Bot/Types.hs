{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module VK.Bot.Types (Pointer (..)) where

import Common.Misc (Label, UpdateId)
import Interface.Class (IBot, IUpdate, MT)
import qualified Interface.Messenger.IBot as IBot
import VK.Bot.Internal (_getInit, _getUpdates, _sendMessage)
import qualified VK.Parse as Parse
import qualified VK.Update as Update

-----------------------------Types---------------------------------------------
data Pointer = Pointer

-- New type wrappers in order to avoid orphan instances
newtype WrapInit = WrapInit Parse.Init

newtype WrapUpdate = WrapUpdate Update.Update deriving newtype (IUpdate)

-----------------------------Instance------------------------------------------
instance IBot Pointer WrapInit WrapUpdate where
  getInit :: MT m => Pointer -> m WrapInit
  getInit _ = WrapInit <$> _getInit

  getUpdateId :: WrapInit -> UpdateId
  getUpdateId (WrapInit ini) = Parse.ts ini

  setUpdateId :: WrapInit -> UpdateId -> WrapInit
  setUpdateId (WrapInit ini) newts = WrapInit $ ini {Parse.ts = newts}

  getUpdates :: MT m => WrapInit -> m ([WrapUpdate], WrapInit)
  getUpdates (WrapInit ini) = do
    (us, newini) <- _getUpdates ini
    return (WrapUpdate <$> us, WrapInit newini)

  sendMessage :: MT m => WrapUpdate -> [Label] -> Int -> m ()
  sendMessage (WrapUpdate u) = _sendMessage u