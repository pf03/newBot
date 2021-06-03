module Messenger.Update.Class where

import Common.Types (ChatId, Command, Message)

class IUpdate update where
  setMessage :: update -> Message -> update
  getMessage :: update -> Maybe Message
  getCommand :: update -> Maybe Command
  getChatId :: update -> ChatId
  hasAttachment :: update -> Bool