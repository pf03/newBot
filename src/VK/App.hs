{-# LANGUAGE MultiParamTypeClasses #-}
--{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DeriveGeneric #-}
--importPriority = 59
module VK.App where

-- Our modules
import qualified Interface.App as App  --60

-- Other modules
import Data.Char
import GHC.Generics
import Data.Aeson
import Common.Misc

-----------------------------Types---------------------------------------------
data Init = Init {server :: String, key :: String, ts :: Int } deriving (Show, Generic)
instance FromJSON Init
instance ToJSON Init
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

data API =  API APIGroup APIName
data APIGroup = Groups | Messages deriving Show
data APIName = GetLongPollServer | Send deriving Show
type Update = (ChatId, Entity)

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