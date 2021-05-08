{-# LANGUAGE MultiParamTypeClasses #-}
--{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}

module Telegram.Update where

-- Our modules
import Interface.Messenger.IUpdate as Update
import Common.Misc
import Interface.MCache

-- Other modules
import Data.Char

-----------------------------Types---------------------------------------------
--Update соответствует одному элементу корневого массива json
type Update = (ChatId, Entity) --deriving Show
type Caption = String
data Entity = Message Message 
    | Command Command 
    | Sticker FileId  --StickerId String или Int для Telegram или VK
    | Animation FileId 
    | Photo FileId (Maybe Caption)
    | Video FileId (Maybe Caption) 
    | Document FileId (Maybe Caption) 
    | Poll {pollId :: StrId, question :: String, options :: [String]}
    | Contact {phoneNumber:: String, firstName::String, mlastName::Maybe String, mvCard::Maybe String}
    | Location Float Float
    | Forward ChatId IntId  --from_chat_id, message_id
    | Other IntId  --возможно этот тип покрывает все остальные, но я об этом не знал
    deriving Show 
--------------------------instance App.Update------------------------------------
-- !!из этой логики нужно убрать cid, он нигде не используется, если так же будет и vk 
instance IUpdate Update  where
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