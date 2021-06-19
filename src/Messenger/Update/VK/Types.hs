{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Messenger.Update.VK.Types where

import Common.Convert ( Convert )
import GHC.Generics (Generic)
import Data.Aeson ( FromJSON, ToJSON )
import Common.Types ( ItemName, Url,IntId,UserId, Key,ChatId, UpdateId,Command, Message )
import Class ( IUpdate ) 
import qualified Messenger.Update.Class as Class 

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

newtype StickerId = StickerId Int deriving newtype (Show, FromJSON, ToJSON, Convert)

newtype OwnerId = OwnerId Int deriving newtype (Show, FromJSON, ToJSON, Convert)

newtype GroupId = GroupId Int deriving newtype (Show, FromJSON, ToJSON, Convert)

data Init = Init {server :: String, key :: String, ts :: UpdateId} deriving (Show, Generic)

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
