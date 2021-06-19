{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Messenger.Update.Telegram.Types where

import Class (IUpdate)
import Common.Convert (Convert)
import Common.Types (ChatId, Command, FileId, IntId, Message, StrId)
import Data.Aeson (FromJSON, ToJSON)
import qualified Messenger.Update.Class as Class

-----------------------------Types---------------------------------------------
-- Update matches one element of the root json array
type Update = (ChatId, Entity)

newtype Caption = Caption String deriving newtype (Show, FromJSON, ToJSON, Convert)

data Entity
  = Message Message
  | Command Command
  | Sticker FileId
  | Animation FileId
  | Photo FileId (Maybe Caption)
  | Video FileId (Maybe Caption)
  | Document FileId (Maybe Caption)
  | Poll {pollId :: StrId, question :: String, options :: [String]}
  | Contact
      { phoneNumber :: String,
        firstName :: String,
        mlastName :: Maybe String,
        mvCard :: Maybe String
      }
  | Location Float Float
  | Forward ChatId IntId
  | Other IntId
  deriving (Show)

--------------------------instance App.Update----------------------------------
instance IUpdate Update where
  setMessage :: Update -> Message -> Update
  setMessage (cid, Message _) message = (cid, Message message)
  setMessage (cid, Command _) message = (cid, Message message)
  setMessage update _ = update

  getMessage :: Update -> Maybe Message
  getMessage (_, Message message) = Just message
  getMessage _ = Nothing

  getCommand :: Update -> Maybe Command
  getCommand (_, Command command) = Just command
  getCommand _ = Nothing

  getChatId :: Update -> ChatId
  getChatId = fst

  hasAttachment :: Update -> Bool
  hasAttachment (_, Message _) = False
  hasAttachment (_, Command _) = False
  hasAttachment (_, _) = True