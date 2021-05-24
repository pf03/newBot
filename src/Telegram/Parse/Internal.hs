{-# LANGUAGE OverloadedStrings #-}

module Telegram.Parse.Internal where

import Common.Misc (ChatId, UpdateId)
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
parseChatLastMessage o = do
  cm <- parseUpdates o
  case cm of
    [] -> fail "No message to reply"
    _ -> return $ last cm

parseUpdates :: Object -> Parser [Update.Update]
parseUpdates = withArraymItem "result" parseUpdate

parseUpdate :: OResultItem -> Parser (Maybe Update.Update)
parseUpdate o = do
  mmessage <- o .:? "message" -- If there is no message field, then we ignore such an update (editing a message, a vote in a poll etc)
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
parseMessage chatId message = do
  mtext <- message .:? "text"
  case mtext of
    Nothing -> do
      mother <- parseOther message -- for universal response, allways Just
      msticker <- parseSticker message
      manimation <- parseAnimation message
      mphoto <- parsePhoto message
      mvideo <- parseVideo message
      mdocument <- parseDocument message
      mpoll <- parsePoll message
      mcontact <- parseContact message
      mlocation <- parseLocation message
      -- The priority of "mother" is determined by its position in the line. If "mother" will be first, other cases will never work
      let mentity = msticker <|> manimation <|> mphoto <|> mvideo <|> mdocument <|> mpoll <|> mcontact <|> mlocation <|> mother
      case mentity of
        Nothing -> fail "Unknown entity type"
        Just entity -> return . Just $ (chatId, entity)
    Just text -> do
      -- command or test
      let emc = Logic.toMessageCommand text
      case emc of
        Left m -> return . Just $ (chatId, Update.Message m)
        Right command -> do
          typeEntities <- withArrayItem "entities" (.: "type") message
          if ("bot_command" :: String) `elem` typeEntities
            then return . Just $ (chatId, Update.Command command)
            else fail "Unknown entity type"

parseForward :: OMessageItem -> Parser (Maybe Update.Entity)
parseForward message = do
  mforwardFrom <- message .:? "forward_from"
  case mforwardFrom of
    Nothing -> return Nothing
    Just forwardFrom -> do
      forwardFromId <- forwardFrom .: "id"
      messageId <- message .: "message_id"
      return $ Just $ Update.Forward forwardFromId messageId

-- allways Just
parseOther :: OMessageItem -> Parser (Maybe Update.Entity)
parseOther o = do
  messageId <- o .: "message_id"
  return $ Just $ Update.Other messageId

parseSticker :: OMessageItem -> Parser (Maybe Update.Entity)
parseSticker = mwithItem "sticker" $ \o -> do
  fileId <- o .: "file_id"
  return $ Update.Sticker fileId

parseAnimation :: OMessageItem -> Parser (Maybe Update.Entity)
parseAnimation = mwithItem "animation" $ \o -> do
  fileId <- o .: "file_id"
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
parsePoll = mwithItem "poll" $ \o -> do
  intId <- o .: "id"
  qu <- o .: "question"
  ops <- withArrayItem "options" (.: "text") o
  return $ Update.Poll intId qu ops

parseContact :: OMessageItem -> Parser (Maybe Update.Entity)
parseContact = mwithItem "contact" $ \o -> do
  pn <- o .: "phone_number"
  fn <- o .: "first_name"
  ln <- o .:? "last_name"
  vc <- o .:? "vcard"
  return $ Update.Contact pn fn ln vc

parseLocation :: OMessageItem -> Parser (Maybe Update.Entity)
parseLocation = mwithItem "location" $ \o -> do
  latitude <- o .: "latitude"
  longitude <- o .: "longitude"
  return $ Update.Location latitude longitude