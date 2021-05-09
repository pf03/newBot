module Interface.Messenger.IUpdate where

-- Our modules
import Common.Misc

class IUpdate update where 
  setMessage :: update -> Message -> update
  getMessage :: update -> Maybe Message
  getCommand :: update -> Maybe Command
  getChatId :: update -> ChatId