module Interface.Messenger.IUpdate where

import Common.Misc
-- import Interface.MCache as Cache

class IUpdate update where 
  setMessage :: update -> Message -> update
  getMessage :: update -> Maybe Message
  getCommand :: update -> Maybe Command
  getChatId :: update -> ChatId