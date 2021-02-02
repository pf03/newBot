{-# LANGUAGE MultiParamTypeClasses #-}
--{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
--importPriority = 59
module VK.App where

import qualified App  --60
import Types  --100
import VK.Types   --100.5

import Data.Char

-------------------------instance App.API---------------------------------------
instance App.API API where
    apiName (API apiGroup apiName) =  let 
        (g:gs) = show apiGroup;
        (n:ns) = show apiName in 
        (toLower g:gs)++ "." ++ (toLower n:ns)
    getPath token api = "/method/" ++ App.apiName api

--------------------------instance App.Update------------------------------------
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