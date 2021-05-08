{-# LANGUAGE MultiParamTypeClasses #-}
--{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DeriveGeneric #-}
--importPriority = 59
module VK.Update where

-- Our modules
import Interface.Messenger.IUpdate as Update  --60

 -- Other modules
-- import Data.Char
-- import GHC.Generics
-- import Data.Aeson
import Common.Misc

-----------------------------Types---------------------------------------------
type Update = (ChatId, Entity)
data Entity = Entity {body:: Either Message Command, attachments :: [Attachment]} deriving Show
data Attachment = 
    Sticker StickerId --StickerId String или Int для Telegram или VK
    -- | Photo AttachmentItem
    -- | Video AttachmentItem
    | Audio OwnerId IntId
    -- | Doc AttachmentItem
    | Wall OwnerId IntId --Key  --тут парс немног отличается
    | Item ItemName OwnerId IntId Key  --photo, video, doc
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
_setMessage (cid, Entity _ attachments)  message = (cid, Entity (Left message) attachments)

_getMessage :: Update -> Maybe Message
_getMessage (_, Entity (Left message) _) = Just message
_getMessage _ = Nothing

_getCommand :: Update -> Maybe Command
_getCommand (_, Entity (Right command) _) = Just command
_getCommand _ = Nothing

_getChatId :: Update -> ChatId
_getChatId = fst