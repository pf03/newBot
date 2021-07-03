{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Parse.Telegram.Internal where

import Common.Types (ChatId, UpdateId)
import Control.Applicative (Alternative ((<|>)))
import Data.Aeson (Object, (.:), (.:?))
import Data.Aeson.Types (Parser)
import qualified Logic.Logic as Logic
import qualified Messenger.Update.Telegram.Types as Update
import Parse.Internal (mWithArrayItem, mWithItem, withArrayItem, withArrayMItem)

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
parseUpdates = withArrayMItem "result" parseUpdate

parseUpdate :: OResultItem -> Parser (Maybe Update.Update)
parseUpdate object = do
  mMessage <- object .:? "message" -- If there is no message field, then we ignore such an update (editing a message, a vote in a poll etc)
  case mMessage of
    Nothing -> return Nothing
    Just message -> do
      chat <- message .: "chat"
      chatId <- chat .: "id"
      mForward <- parseForward message
      case mForward of
        Just forward -> return . Just $ (chatId, forward)
        Nothing -> parseMessage chatId message

parseMessage :: ChatId -> Object -> Parser (Maybe Update.Update)
parseMessage chatId object = do
  mText <- object .:? "text"
  case mText of
    Nothing -> do
      mOther <- parseOther object -- for universal response, always Just
      mSticker <- parseSticker object
      mAnimation <- parseAnimation object
      mPhoto <- parsePhoto object
      mVideo <- parseVideo object
      mDocument <- parseDocument object
      mPoll <- parsePoll object
      mContact <- parseContact object
      mLocation <- parseLocation object
      -- The priority of "mother" is determined by its position in the line. If "mother" will be first, other cases will never work
      let mEntity = mSticker <|> mAnimation <|> mPhoto <|> mVideo <|> mDocument <|> mPoll <|> mContact <|> mLocation <|> mOther
      case mEntity of
        Nothing -> fail "Unknown entity type"
        Just entity -> return . Just $ (chatId, entity)
    Just text -> do
      let eMessageCommand = Logic.toMessageCommand text
      case eMessageCommand of
        Left message -> return . Just $ (chatId, Update.Message message)
        Right command -> return . Just $ (chatId, Update.Command command)

parseForward :: OMessageItem -> Parser (Maybe Update.Entity)
parseForward object = do
  mForwardFrom <- object .:? "forward_from"
  case mForwardFrom of
    Nothing -> return Nothing
    Just forwardFrom -> do
      forwardFromId <- forwardFrom .: "id"
      messageId <- object .: "message_id"
      return $ Just $ Update.Forward forwardFromId messageId

-- always Just
parseOther :: OMessageItem -> Parser (Maybe Update.Entity)
parseOther object = do
  messageId <- object .: "message_id"
  return $ Just $ Update.Other messageId

parseSticker :: OMessageItem -> Parser (Maybe Update.Entity)
parseSticker = mWithItem "sticker" $ \object -> do
  fileId <- object .: "file_id"
  return $ Update.Sticker fileId

parseAnimation :: OMessageItem -> Parser (Maybe Update.Entity)
parseAnimation = mWithItem "animation" $ \object -> do
  fileId <- object .: "file_id"
  return $ Update.Animation fileId

parsePhoto :: OMessageItem -> Parser (Maybe Update.Entity)
parsePhoto message = do
  mPhotos <- mWithArrayItem "photo" (.: "file_id") message
  mCaption <- message .:? "caption"
  case mPhotos of
    Nothing -> return Nothing
    Just photos -> return $ Just $ Update.Photo (last photos) mCaption

parseVideo :: OMessageItem -> Parser (Maybe Update.Entity)
parseVideo message = do
  mVideo <- message .:? "video"
  case mVideo of
    Nothing -> return Nothing
    Just video -> do
      mCaption <- message .:? "caption"
      fileId <- video .: "file_id"
      return $ Just $ Update.Video fileId mCaption

parseDocument :: OMessageItem -> Parser (Maybe Update.Entity)
parseDocument message = do
  mFile <- message .:? "document"
  case mFile of
    Nothing -> return Nothing
    Just file -> do
      mCaption <- message .:? "caption"
      fileId <- file .: "file_id"
      return $ Just $ Update.Document fileId mCaption

parsePoll :: OMessageItem -> Parser (Maybe Update.Entity)
parsePoll = mWithItem "poll" $ \object -> do
  pollId <- object .: "id"
  question <- object .: "question"
  options <- withArrayItem "options" (.: "text") object
  return $ Update.Poll {..}

parseContact :: OMessageItem -> Parser (Maybe Update.Entity)
parseContact = mWithItem "contact" $ \object -> do
  phoneNumber <- object .: "phone_number"
  firstName <- object .: "first_name"
  mLastName <- object .:? "last_name"
  mVCard <- object .:? "vcard"
  return $ Update.Contact {..}

parseLocation :: OMessageItem -> Parser (Maybe Update.Entity)
parseLocation = mWithItem "location" $ \object -> do
  latitude <- object .: "latitude"
  longitude <- object .: "longitude"
  return $ Update.Location latitude longitude