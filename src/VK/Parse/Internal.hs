module VK.Parse.Internal where

import Common.Misc (UpdateId, UserId)
import Data.Aeson (Object, (.:), (.:?))
import Data.Aeson.Types (Parser)
import Data.Maybe (fromMaybe)
import Data.Text (pack)
import qualified Logic.Logic as Logic
import Logic.Parse.Internal (mwithArrayItem, parseJSONo, withArraymItem)
import Text.Read (readEither)
import qualified VK.Update as Update

parseUpdateId :: Object -> Parser (Maybe UpdateId)
parseUpdateId o = do
  mstr <- o .:? "ts"
  case mstr of
    Nothing -> return Nothing
    Just str -> do
      let euid = readEither str
      case euid of
        Left e -> fail (e ++ ": " ++ str)
        Right uid -> return $ Just uid

parseInit :: Object -> Parser Update.Init
parseInit o = do
  response <- o .: "response"
  parseJSONo response

parseUpdates :: Object -> Parser [Update.Update]
parseUpdates = withArraymItem "updates" parseUpdate

parseUpdate :: Object -> Parser (Maybe Update.Update)
parseUpdate o = do
  t <- o .: "type" :: Parser String
  case t of
    "message_new" -> do
      obj <- o .: "object"
      userId <- obj .: "user_id" :: Parser UserId
      text <- obj .: "body"
      let emc = Logic.toMessageCommand text
      mas <- mwithArrayItem "attachments" parseAttachment obj
      let as = fromMaybe [] mas
      return $ Just (userId, Update.Entity emc as)
    _ -> return Nothing

parseAttachment :: Object -> Parser Update.Attachment
parseAttachment o = do
  t <- o .: "type" :: Parser String
  case t of
    "sticker" -> parseSticker o
    "audio" -> parseAudio o
    "photo" -> parseAttachmentItem "photo" o
    "video" -> parseAttachmentItem "video" o
    "doc" -> parseAttachmentItem "doc" o
    "wall" -> parseWall o
    "link" -> parseLink o
    _ -> fail "Unknown attachment"

parseAttachmentItem :: String -> Object -> Parser Update.Attachment
parseAttachmentItem str o = do
  item <- o .: pack str
  itemId <- item .: "id"
  ownerId <- item .: "owner_id"
  accessKey <- item .: "access_key"
  return $ Update.Item str ownerId itemId accessKey

parseSticker :: Object -> Parser Update.Attachment
parseSticker o = do
  sticker <- o .: "sticker"
  stickerId <- sticker .: "id"
  return . Update.Sticker $ stickerId

parseAudio :: Object -> Parser Update.Attachment
parseAudio o = do
  audio <- o .: "audio"
  audioId <- audio .: "id"
  ownerId <- audio .: "owner_id"
  return $ Update.Audio ownerId audioId

parseWall :: Object -> Parser Update.Attachment
parseWall o = do
  wall <- o .: "wall"
  wallId <- wall .: "id"
  ownerId <- wall .: "to_id"
  return $ Update.Wall ownerId wallId

parseLink :: Object -> Parser Update.Attachment
parseLink o = do
  item <- o .: "link"
  url <- item .: "url"
  return $ Update.Link url