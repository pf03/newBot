{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Telegram.Update where

import Common.Misc (ChatId, Command, FileId, IntId, Message, StrId)
import Interface.Class ( IUpdate ) 
import qualified Interface.Messenger.IUpdate as IUpdate

-----------------------------Types---------------------------------------------
-- Update matches one element of the root json array
type Update = (ChatId, Entity)

type Caption = String

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
  setMessage u _ = u

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