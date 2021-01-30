{-# LANGUAGE FlexibleInstances #-}
--importPriority = 31
module VK.Logic where 

import Types  --100
import VK.Types --99
import qualified App  --60

instance App.Update Update  where
    setMessage = _setMessage
    getMessage = _getMessage
    getCommand = _getCommand
    getChatId = _getChatId

_setMessage :: Update -> Message -> Update
_setMessage (cid, Entity _ attachments)  message = (cid, Entity (Left message) attachments)

_getMessage :: Update -> Maybe Message
_getMessage (_, Entity (Left message) _) = Just message
_getMessage _ = Nothing

_getCommand :: Update -> Maybe Command
_getCommand (_, Entity (Right command) _) = Just command
_getCommand _ = Nothing

-- _getEntity :: Update -> Entity  -- тут попахивает мультипараметрическим классом
-- _getEntity = snd

_getChatId :: Update -> ChatId
_getChatId = fst