{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Telegram.Bot.Types (Pointer (..)) where

import Common.Misc ( Label, UpdateId )
import Interface.Class (IBot, IUpdate, MT)
import qualified Interface.Messenger.IBot as IBot
import Telegram.Bot.Internal (_getUpdateId, _getUpdates, _sendMessage)
import qualified Telegram.Update as Update

-----------------------------Types---------------------------------------------
data Pointer = Pointer

-- New type wrappers in order to avoid orphan instances
newtype Init = Init UpdateId

newtype WrapUpdate = WrapUpdate Update.Update deriving newtype (IUpdate)

-----------------------------Instance------------------------------------------
instance IBot Pointer Init WrapUpdate where
  getInit :: MT m => Pointer -> m Init
  getInit _ = Init <$> _getUpdateId

  getUpdateId :: Init -> UpdateId
  getUpdateId (Init uid) = uid

  setUpdateId :: Init -> UpdateId -> Init
  setUpdateId _ newuid = Init newuid

  getUpdates :: MT m => Init -> m ([WrapUpdate], Init)
  getUpdates (Init uid) = do
    (us, Just newuid) <- _getUpdates . Just $ uid
    return (WrapUpdate <$> us, Init newuid)

  sendMessage :: MT m => WrapUpdate -> [Label] -> Int -> m ()
  sendMessage (WrapUpdate u) ls = _sendMessage u ls