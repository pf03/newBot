module Messenger.Update.Class where

import Common.Types (ChatId, Command, Message)

class IUpdate update where
  setMessage :: update -> Message -> update
  getCommand :: update -> Maybe Command
  getChatId :: update -> ChatId