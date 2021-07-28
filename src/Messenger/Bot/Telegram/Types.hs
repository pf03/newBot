{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies          #-}

module Messenger.Bot.Telegram.Types where

import Class (IUpdate)
import Common.Types (UpdateId)
import qualified Messenger.Update.Telegram.Types as Update

-- New type wrappers in order to avoid orphan instances
newtype Init = Init (Maybe UpdateId)

newtype WrapUpdate = WrapUpdate Update.Update deriving newtype (IUpdate)

data API
  = GetUpdates
  | SendMessage
  | SendSticker
  | SendAnimation
  | SendPhoto
  | SendVideo
  | SendDocument
  | SendPoll
  | SendContact
  | SendLocation
  | CopyMessage
  | ForwardMessage
  deriving (Show)