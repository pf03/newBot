{-# LANGUAGE MultiParamTypeClasses #-}
--{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}

--importPriority = 59
module Telegram.App where

import qualified App
import Data.Char
import Types --100
import Telegram.Types 

-------------------------instance App.API---------------------------------------
instance App.API API where
    apiName api = let 
        (x:xs) = show api 
        in (toLower x):xs
    getPath token api = "/bot"++ token ++"/" ++ App.apiName api

--------------------------instance App.Update------------------------------------
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