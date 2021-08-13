module Messenger.Bot.Telegram.Types where

import Common.Types (UpdateId)

newtype Init = Init (Maybe UpdateId)

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