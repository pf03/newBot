module Parse.Telegram.Internal where

import Common.Types
  ( ChatId,
    MessageOrCommand (CommandEntity, MessageEntity),
    UpdateId,
  )
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
      maybe (parseMessage chatId message) (\forward -> return . Just $ (chatId, forward)) mForward

parseMessage :: ChatId -> Object -> Parser (Maybe Update.Update)
parseMessage chatId object = do
  mText <- object .:? "text"
  case mText of
    Nothing -> do
      mOther <- parseOther object
      mSticker <- parseSticker object
      mAnimation <- parseAnimation object
      mPhoto <- parsePhoto object
      mVideo <- parseVideo object
      mDocument <- parseDocument object
      mPoll <- parsePoll object
      mContact <- parseContact object
      mLocation <- parseLocation object
      let mEntity = mSticker <|> mAnimation <|> mPhoto <|> mVideo <|> mDocument <|> mPoll <|> mContact <|> mLocation <|> mOther
      maybe (fail "Unknown entity type") (\entity -> return . Just $ (chatId, entity)) mEntity
    Just text -> do
      let messageOrCommand = Logic.toMessageCommand text
      case messageOrCommand of
        MessageEntity message -> return . Just $ (chatId, Update.Message message)
        CommandEntity command -> return . Just $ (chatId, Update.Command command)

parseForward :: OMessageItem -> Parser (Maybe Update.Entity)
parseForward object = do
  mForwardFrom <- object .:? "forward_from"
  case mForwardFrom of
    Nothing -> return Nothing
    Just forwardFrom -> do
      forwardFromId <- forwardFrom .: "id"
      messageId <- object .: "message_id"
      return $ Just $ Update.Forward forwardFromId messageId

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