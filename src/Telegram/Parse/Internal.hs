{-# LANGUAGE OverloadedStrings #-}

module Telegram.Parse.Internal where

import Common.Misc (ChatId, UpdateId)
import Control.Applicative (Alternative ((<|>)))
import Data.Aeson (Object, (.:), (.:?))
import Data.Aeson.Types (Parser)
import qualified Logic.Logic as Logic
import Logic.Parse.Internal (_mwithArrayItem, _mwithItem, _withArrayItem, _withArraymItem)
import Telegram.Update (Entity (..), Update)

type OResultItem = Object

type OMessageItem = Object

_parseUpdateIds :: Object -> Parser [UpdateId]
_parseUpdateIds = _withArrayItem "result" (.: "update_id")

_parseChatLastMessage :: Object -> Parser Update
_parseChatLastMessage o = do
  cm <- _parseUpdates o
  case cm of
    [] -> fail "No message to reply"
    _ -> return $ last cm

_parseUpdates :: Object -> Parser [Update]
_parseUpdates = _withArraymItem "result" _parseUpdate

_parseUpdate :: OResultItem -> Parser (Maybe Update)
_parseUpdate o = do
  mmessage <- o .:? "message" -- If there is no message field, then we ignore such an update (editing a message, a vote in a poll etc)
  case mmessage of
    Nothing -> return Nothing
    Just message -> do
      chat <- message .: "chat"
      chatId <- chat .: "id"
      mforward <- _parseForward message
      case mforward of
        Just forward -> return . Just $ (chatId, forward)
        Nothing -> _parseMessage chatId message

_parseMessage :: ChatId -> Object -> Parser (Maybe Update)
_parseMessage chatId message = do
  mtext <- message .:? "text"
  case mtext of
    Nothing -> do
      mother <- _parseOther message -- for universal response, allways Just
      msticker <- _parseSticker message
      manimation <- _parseAnimation message
      mphoto <- _parsePhoto message
      mvideo <- _parseVideo message
      mdocument <- _parseDocument message
      mpoll <- _parsePoll message
      mcontact <- _parseContact message
      mlocation <- _parseLocation message
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
          typeEntities <- _withArrayItem "entities" (.: "type") message
          if ("bot_command" :: String) `elem` typeEntities
            then return . Just $ (chatId, Command command)
            else fail "Unknown entity type"

_parseForward :: OMessageItem -> Parser (Maybe Entity)
_parseForward message = do
  mforwardFrom <- message .:? "forward_from"
  case mforwardFrom of
    Nothing -> return Nothing
    Just forwardFrom -> do
      forwardFromId <- forwardFrom .: "id"
      messageId <- message .: "message_id"
      return $ Just $ Forward forwardFromId messageId

-- allways Just
_parseOther :: OMessageItem -> Parser (Maybe Entity)
_parseOther o = do
  messageId <- o .: "message_id"
  return $ Just $ Other messageId

_parseSticker :: OMessageItem -> Parser (Maybe Entity)
_parseSticker = _mwithItem "sticker" $ \o -> do
  fileId <- o .: "file_id"
  return $ Sticker fileId

_parseAnimation :: OMessageItem -> Parser (Maybe Entity)
_parseAnimation = _mwithItem "animation" $ \o -> do
  fileId <- o .: "file_id"
  return $ Animation fileId

_parsePhoto :: OMessageItem -> Parser (Maybe Entity)
_parsePhoto message = do
  mphotos <- _mwithArrayItem "photo" (.: "file_id") message
  mcaption <- message .:? "caption"
  case mphotos of
    Nothing -> return Nothing
    Just photos -> return $ Just $ Photo (last photos) mcaption

_parseVideo :: OMessageItem -> Parser (Maybe Entity)
_parseVideo message = do
  mvideo <- message .:? "video"
  case mvideo of
    Nothing -> return Nothing
    Just video -> do
      mcaption <- message .:? "caption"
      fileId <- video .: "file_id"
      return $ Just $ Video fileId mcaption

_parseDocument :: OMessageItem -> Parser (Maybe Entity)
_parseDocument message = do
  mfile <- message .:? "document"
  case mfile of
    Nothing -> return Nothing
    Just file -> do
      mcaption <- message .:? "caption"
      fileId <- file .: "file_id"
      return $ Just $ Document fileId mcaption

_parsePoll :: OMessageItem -> Parser (Maybe Entity)
_parsePoll = _mwithItem "poll" $ \o -> do
  intId <- o .: "id"
  qu <- o .: "question"
  ops <- _withArrayItem "options" (.: "text") o
  return $ Poll intId qu ops

_parseContact :: OMessageItem -> Parser (Maybe Entity)
_parseContact = _mwithItem "contact" $ \o -> do
  pn <- o .: "phone_number"
  fn <- o .: "first_name"
  ln <- o .:? "last_name"
  vc <- o .:? "vcard"
  return $ Contact pn fn ln vc

_parseLocation :: OMessageItem -> Parser (Maybe Entity)
_parseLocation = _mwithItem "location" $ \o -> do
  latitude <- o .: "latitude"
  longitude <- o .: "longitude"
  return $ Location latitude longitude