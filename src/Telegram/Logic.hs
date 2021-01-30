{-# LANGUAGE FlexibleInstances #-}
--importPriority = 31
module Telegram.Logic where 

import Types --100
import Telegram.Types  --99
import qualified App

-- --формируем ответ из update и ответа на сообщение или команду
-- answerUpdate :: (App.Main _pointer _init update) => update -> Either Message Command -> update
-- answerUpdate = undefined 


-- !!из этой логики нужно убрать cid, он нигде не используется, если так же будет и vk 
instance App.Update Update  where
    setMessage = _setMessage
    getMessage = _getMessage
    getCommand = _getCommand
    getChatId = _getChatId

--линза не должна менять структуру, а только значение
--одно исключение - команду меняем на сообщение, т. к. пользователю нельзя послать команду
_setMessage :: Update -> Message -> Update
_setMessage (cid, Message _) message = (cid, Message message)
--не знаю, можно ли считать caption сообщением?
_setMessage (cid, Photo fileId (Just _) ) message = (cid, Photo fileId (Just message)) 
_setMessage (cid, Video fileId (Just _) ) message = (cid, Video fileId (Just message)) 
_setMessage (cid, Command _ ) message = (cid, Message message) 
_setMessage u _ = u
 
_getMessage :: Update -> Maybe Message
_getMessage (_, Message message) = Just message
_getMessage (_, Photo _ (Just message) ) = Just message
_getMessage (_, Video _ (Just message) ) = Just message
_getMessage _ = Nothing

_getCommand :: Update -> Maybe Command
_getCommand (_, Command command) = Just command
_getCommand _ = Nothing

-- _getEntity :: Update -> Entity
-- _getEntity = snd

_getChatId :: Update -> ChatId
_getChatId = fst