{-# LANGUAGE OverloadedStrings #-}

module Telegram.Parse.Internal where

import Common.Misc (ChatId, UpdateId)
import Control.Applicative (Alternative ((<|>)))
import Data.Aeson (Object, (.:), (.:?))
import Data.Aeson.Types (Parser)
import qualified Logic.Logic as Logic
import Logic.Parse.Internal (mwithArrayItem, mwithItem, withArrayItem, withArraymItem)
import Telegram.Update (Entity (..), Update)

type OResultItem = Object

type OMessageItem = Object

parseUpdateIds :: Object -> Parser [UpdateId]
parseUpdateIds = withArrayItem "result" (.: "update_id")

parseChatLastMessage :: Object -> Parser Update
parseChatLastMessage o = do
  cm <- parseUpdates o
  case cm of
    [] -> fail "No message to reply"
    _ -> return $ last cm

parseUpdates :: Object -> Parser [Update]
parseUpdates = withArraymItem "result" parseUpdate

parseUpdate :: OResultItem -> Parser (Maybe Update)
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

parseMessage :: ChatId -> Object -> Parser (Maybe Update)
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
        Left m -> return . Just $ (chatId, Message m)
        Right command -> do
          typeEntities <- withArrayItem "entities" (.: "type") message
          if ("bot_command" :: String) `elem` typeEntities
            then return . Just $ (chatId, Command command)
            else fail "Unknown entity type"

parseForward :: OMessageItem -> Parser (Maybe Entity)
parseForward message = do
  mforwardFrom <- message .:? "forward_from"
  case mforwardFrom of
    Nothing -> return Nothing
    Just forwardFrom -> do
      forwardFromId <- forwardFrom .: "id"
      messageId <- message .: "message_id"
      return $ Just $ Forward forwardFromId messageId

-- allways Just
parseOther :: OMessageItem -> Parser (Maybe Entity)
parseOther o = do
  messageId <- o .: "message_id"
  return $ Just $ Other messageId

parseSticker :: OMessageItem -> Parser (Maybe Entity)
parseSticker = mwithItem "sticker" $ \o -> do
  fileId <- o .: "file_id"
  return $ Sticker fileId

parseAnimation :: OMessageItem -> Parser (Maybe Entity)
parseAnimation = mwithItem "animation" $ \o -> do
  fileId <- o .: "file_id"
  return $ Animation fileId

parsePhoto :: OMessageItem -> Parser (Maybe Entity)
parsePhoto message = do
  mphotos <- mwithArrayItem "photo" (.: "file_id") message
  mcaption <- message .:? "caption"
  case mphotos of
    Nothing -> return Nothing
    Just photos -> return $ Just $ Photo (last photos) mcaption

parseVideo :: OMessageItem -> Parser (Maybe Entity)
parseVideo message = do
  mvideo <- message .:? "video"
  case mvideo of
    Nothing -> return Nothing
    Just video -> do
      mcaption <- message .:? "caption"
      fileId <- video .: "file_id"
      return $ Just $ Video fileId mcaption

parseDocument :: OMessageItem -> Parser (Maybe Entity)
parseDocument message = do
  mfile <- message .:? "document"
  case mfile of
    Nothing -> return Nothing
    Just file -> do
      mcaption <- message .:? "caption"
      fileId <- file .: "file_id"
      return $ Just $ Document fileId mcaption

parsePoll :: OMessageItem -> Parser (Maybe Entity)
parsePoll = mwithItem "poll" $ \o -> do
  intId <- o .: "id"
  qu <- o .: "question"
  ops <- withArrayItem "options" (.: "text") o
  return $ Poll intId qu ops

parseContact :: OMessageItem -> Parser (Maybe Entity)
parseContact = mwithItem "contact" $ \o -> do
  pn <- o .: "phone_number"
  fn <- o .: "first_name"
  ln <- o .:? "last_name"
  vc <- o .:? "vcard"
  return $ Contact pn fn ln vc

parseLocation :: OMessageItem -> Parser (Maybe Entity)
parseLocation = mwithItem "location" $ \o -> do
  latitude <- o .: "latitude"
  longitude <- o .: "longitude"
  return $ Location latitude longitude