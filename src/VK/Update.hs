{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs #-}
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

    setMessage :: Update -> Message -> Update
    setMessage (cid, Entity _ as)  message = (cid, Entity (Left message) as)

    getMessage :: Update -> Maybe Message
    getMessage (_, Entity (Left message) _) = Just message
    getMessage _ = Nothing

    getCommand :: Update -> Maybe Command
    getCommand (_, Entity (Right command) _) = Just command
    getCommand _ = Nothing

    getChatId :: Update -> ChatId
    getChatId = fst