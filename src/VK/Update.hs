{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
module VK.Update where

-- Our modules
import Interface.Messenger.IUpdate as Update  --60
import Common.Misc

-----------------------------Types---------------------------------------------
type Update = (ChatId, Entity)
data Entity = Entity {body:: Either Message Command, attachments :: [Attachment]} deriving Show
data Attachment = 
    Sticker StickerId
    | Audio OwnerId IntId
    | Wall OwnerId IntId -- parser differs from Audio
    | Item ItemName OwnerId IntId Key  -- photo, video, doc
    | Link Url
    deriving Show 
type StickerId = Int
type OwnerId = Int
type GroupId = Int

--------------------------instance App.Update------------------------------------
instance IUpdate Update  where
    setMessage = _setMessage
    getMessage = _getMessage
    getCommand = _getCommand
    getChatId = _getChatId

_setMessage :: Update -> Message -> Update
_setMessage (cid, Entity _ as)  message = (cid, Entity (Left message) as)

_getMessage :: Update -> Maybe Message
_getMessage (_, Entity (Left message) _) = Just message
_getMessage _ = Nothing

_getCommand :: Update -> Maybe Command
_getCommand (_, Entity (Right command) _) = Just command
_getCommand _ = Nothing

_getChatId :: Update -> ChatId
_getChatId = fst