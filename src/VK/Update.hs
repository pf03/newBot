{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE DeriveGeneric #-}

module VK.Update where

import GHC.Generics (Generic)
import Data.Aeson ( FromJSON, ToJSON )
import Common.Types (ChatId, Command, IntId, ItemName, Key, Message, Url)
import Interface.Class ( IUpdate ) 
import qualified Interface.Messenger.IUpdate as Update 

-----------------------------Types---------------------------------------------
type Update = (ChatId, Entity)

data Entity = Entity {body :: Either Message Command, attachments :: [Attachment]} deriving (Show)

data Attachment
  = Sticker StickerId
  | Audio OwnerId IntId
  | Wall OwnerId IntId -- parser differs from Audio
  | Item ItemName OwnerId IntId Key -- photo, video, doc
  | Link Url
  deriving (Show)

type StickerId = Int

type OwnerId = Int

type GroupId = Int

data Init = Init {server :: String, key :: String, ts :: Int} deriving (Show, Generic)

instance FromJSON Init

instance ToJSON Init

--------------------------instance App.Update------------------------------------
instance IUpdate Update where
  setMessage :: Update -> Message -> Update
  setMessage (chatId, Entity _ attachments0) message = (chatId, Entity (Left message) attachments0)

  getMessage :: Update -> Maybe Message
  getMessage (_, Entity (Left message) _) = Just message
  getMessage _ = Nothing

  getCommand :: Update -> Maybe Command
  getCommand (_, Entity (Right command) _) = Just command
  getCommand _ = Nothing

  getChatId :: Update -> ChatId
  getChatId = fst

  hasAttachment :: Update -> Bool
  hasAttachment (_, Entity _ []) = False
  hasAttachment (_, Entity _ _) = True
