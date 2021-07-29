{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Messenger.Update.VK.Types where

import Messenger.Update.Class (IUpdate)
import Common.Convert (Convert)
import Common.Types (ChatId, Command, IntId, ItemName, Key, Message, UpdateId, Url)
import Data.Aeson (FromJSON, ToJSON)
import GHC.Generics (Generic)
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

  getCommand :: Update -> Maybe Command
  getCommand (_, Entity (Right command) _) = Just command
  getCommand _ = Nothing

  getChatId :: Update -> ChatId
  getChatId = fst
