module Parse.VK.Internal where

import Common.Types (UpdateId, UserId)
import Data.Aeson (Object, (.:), (.:?))
import Data.Aeson.Types (Parser)
import Data.Maybe (fromMaybe)
import Data.Text (pack)
import qualified Logic.Logic as Logic
import Parse.Internal (mwithArrayItem, parseJSONo, withArraymItem)
import Text.Read (readEither)
import qualified VK.Update as Update

parseUpdateId :: Object -> Parser (Maybe UpdateId)
parseUpdateId object = do
  mstr <- object .:? "ts"
  case mstr of
    Nothing -> return Nothing
    Just str -> do
      let eUpdateId = readEither str
      case eUpdateId of
        Left err -> fail (err ++ ": " ++ str)
        Right updateId -> return $ Just updateId

parseInit :: Object -> Parser Update.Init
parseInit object = do
  response <- object .: "response"
  parseJSONo response

parseUpdates :: Object -> Parser [Update.Update]
parseUpdates = withArraymItem "updates" parseUpdate

parseUpdate :: Object -> Parser (Maybe Update.Update)
parseUpdate object = do
  updateType <- object .: "type" :: Parser String
  case updateType of
    "message_new" -> do
      object1 <- object .: "object"
      userId <- object1 .: "user_id" :: Parser UserId
      body <- object1 .: "body"
      let eMessageCommand = Logic.toMessageCommand body
      mAttachments <- mwithArrayItem "attachments" parseAttachment object1
      let attachments = fromMaybe [] mAttachments
      return $ Just (userId, Update.Entity eMessageCommand attachments)
    _ -> return Nothing

parseAttachment :: Object -> Parser Update.Attachment
parseAttachment object = do
  t <- object .: "type" :: Parser String
  case t of
    "sticker" -> parseSticker object
    "audio" -> parseAudio object
    "photo" -> parseAttachmentItem "photo" object
    "video" -> parseAttachmentItem "video" object
    "doc" -> parseAttachmentItem "doc" object
    "wall" -> parseWall object
    "link" -> parseLink object
    _ -> fail "Unknown attachment"

parseAttachmentItem :: String -> Object -> Parser Update.Attachment
parseAttachmentItem str object = do
  item <- object .: pack str
  itemId <- item .: "id"
  ownerId <- item .: "owner_id"
  accessKey <- item .: "access_key"
  return $ Update.Item str ownerId itemId accessKey

parseSticker :: Object -> Parser Update.Attachment
parseSticker object = do
  sticker <- object .: "sticker"
  stickerId <- sticker .: "id"
  return . Update.Sticker $ stickerId

parseAudio :: Object -> Parser Update.Attachment
parseAudio object = do
  audio <- object .: "audio"
  audioId <- audio .: "id"
  ownerId <- audio .: "owner_id"
  return $ Update.Audio ownerId audioId

parseWall :: Object -> Parser Update.Attachment
parseWall object = do
  wall <- object .: "wall"
  wallId <- wall .: "id"
  ownerId <- wall .: "to_id"
  return $ Update.Wall ownerId wallId

parseLink :: Object -> Parser Update.Attachment
parseLink object = do
  item <- object .: "link"
  url <- item .: "url"
  return $ Update.Link url