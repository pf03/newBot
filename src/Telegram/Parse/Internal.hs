{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Telegram.Parse.Internal where

import Common.Types (ChatId, UpdateId)
import Control.Applicative (Alternative ((<|>)))
import Data.Aeson (Object, (.:), (.:?))
import Data.Aeson.Types (Parser)
import qualified Logic.Logic as Logic
import Logic.Parse.Internal (mwithArrayItem, mwithItem, withArrayItem, withArraymItem)
import qualified Telegram.Update as Update

type OResultItem = Object

type OMessageItem = Object

parseUpdateIds :: Object -> Parser [UpdateId]
parseUpdateIds = withArrayItem "result" (.: "update_id")

parseChatLastMessage :: Object -> Parser Update.Update
parseChatLastMessage object = do
  updates <- parseUpdates object
  case updates of
    [] -> fail "No message to reply"
    _ -> return $ last updates

parseUpdates :: Object -> Parser [Update.Update]
parseUpdates = withArraymItem "result" parseUpdate

parseUpdate :: OResultItem -> Parser (Maybe Update.Update)
parseUpdate object = do
  mmessage <- object .:? "message" -- If there is no message field, then we ignore such an update (editing a message, a vote in a poll etc)
  case mmessage of
    Nothing -> return Nothing
    Just message -> do
      chat <- message .: "chat"
      chatId <- chat .: "id"
      mforward <- parseForward message
      case mforward of
        Just forward -> return . Just $ (chatId, forward)
        Nothing -> parseMessage chatId message

parseMessage :: ChatId -> Object -> Parser (Maybe Update.Update)
parseMessage chatId object = do
  mtext <- object .:? "text"
  case mtext of
    Nothing -> do
      mother <- parseOther object -- for universal response, allways Just
      msticker <- parseSticker object
      manimation <- parseAnimation object
      mphoto <- parsePhoto object
      mvideo <- parseVideo object
      mdocument <- parseDocument object
      mpoll <- parsePoll object
      mcontact <- parseContact object
      mlocation <- parseLocation object
      -- The priority of "mother" is determined by its position in the line. If "mother" will be first, other cases will never work
      let mentity = msticker <|> manimation <|> mphoto <|> mvideo <|> mdocument <|> mpoll <|> mcontact <|> mlocation <|> mother
      case mentity of
        Nothing -> fail "Unknown entity type"
        Just entity -> return . Just $ (chatId, entity)
    Just text -> do
      let eMessageCommand = Logic.toMessageCommand text
      case eMessageCommand of
        Left message -> return . Just $ (chatId, Update.Message message)
        Right command -> return . Just $ (chatId, Update.Command command)

parseForward :: OMessageItem -> Parser (Maybe Update.Entity)
parseForward object = do
  mforwardFrom <- object .:? "forward_from"
  case mforwardFrom of
    Nothing -> return Nothing
    Just forwardFrom -> do
      forwardFromId <- forwardFrom .: "id"
      messageId <- object .: "message_id"
      return $ Just $ Update.Forward forwardFromId messageId

-- allways Just
parseOther :: OMessageItem -> Parser (Maybe Update.Entity)
parseOther object = do
  messageId <- object .: "message_id"
  return $ Just $ Update.Other messageId

parseSticker :: OMessageItem -> Parser (Maybe Update.Entity)
parseSticker = mwithItem "sticker" $ \object -> do
  fileId <- object .: "file_id"
  return $ Update.Sticker fileId

parseAnimation :: OMessageItem -> Parser (Maybe Update.Entity)
parseAnimation = mwithItem "animation" $ \object -> do
  fileId <- object .: "file_id"
  return $ Update.Animation fileId

parsePhoto :: OMessageItem -> Parser (Maybe Update.Entity)
parsePhoto message = do
  mphotos <- mwithArrayItem "photo" (.: "file_id") message
  mcaption <- message .:? "caption"
  case mphotos of
    Nothing -> return Nothing
    Just photos -> return $ Just $ Update.Photo (last photos) mcaption

parseVideo :: OMessageItem -> Parser (Maybe Update.Entity)
parseVideo message = do
  mvideo <- message .:? "video"
  case mvideo of
    Nothing -> return Nothing
    Just video -> do
      mcaption <- message .:? "caption"
      fileId <- video .: "file_id"
      return $ Just $ Update.Video fileId mcaption

parseDocument :: OMessageItem -> Parser (Maybe Update.Entity)
parseDocument message = do
  mfile <- message .:? "document"
  case mfile of
    Nothing -> return Nothing
    Just file -> do
      mcaption <- message .:? "caption"
      fileId <- file .: "file_id"
      return $ Just $ Update.Document fileId mcaption

parsePoll :: OMessageItem -> Parser (Maybe Update.Entity)
parsePoll = mwithItem "poll" $ \object -> do
  pollId <- object .: "id"
  question <- object .: "question"
  options <- withArrayItem "options" (.: "text") object
  return $ Update.Poll {..}

parseContact :: OMessageItem -> Parser (Maybe Update.Entity)
parseContact = mwithItem "contact" $ \object -> do
  phoneNumber <- object .: "phone_number"
  firstName <- object .: "first_name"
  mlastName <- object .:? "last_name"
  mvCard <- object .:? "vcard"
  return $ Update.Contact {..}

parseLocation :: OMessageItem -> Parser (Maybe Update.Entity)
parseLocation = mwithItem "location" $ \object -> do
  latitude <- object .: "latitude"
  longitude <- object .: "longitude"
  return $ Update.Location latitude longitude