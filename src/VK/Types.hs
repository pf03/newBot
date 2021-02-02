{-# LANGUAGE DeriveGeneric #-}
--importPriority = 99
module VK.Types where

import qualified App --60 направление импорта!!!! перенести в VK.App

import Types  --100
import Data.Aeson
import GHC.Generics

data API =  API APIGroup APIName

data APIGroup = Groups | Messages deriving Show
data APIName = GetLongPollServer | Send deriving Show

--------------------------------Parse && Logic----------------------------------------------------------

type GroupId = Int 
type Version = String 


data Init = Init {server :: String, key :: String, ts :: Int } deriving (Show, Generic)

instance FromJSON Init
instance ToJSON Init

-- instance App.Auth Auth where 
--     fauth _ = ()

data Pointer = Pointer --синглтон для указания на текущее приложение

type StickerId = Int
type OwnerId = Int
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

--data AttachmentItem = Item ItemName OwnerId IntId Key deriving Show 

    
-- | Video FileId 
    



--Update соответствует одному элементу корневого массива json
--data Update = Update {userId :: UserId, body:: Message, attachments :: [Entity]} deriving Show


