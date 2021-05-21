module VK.Parse.Internal where

import Common.Misc ( UserId, UpdateId ) 
import Data.Aeson ( (.:), (.:?), Object )
import Data.Aeson.Types (Parser)
import Data.Maybe (fromMaybe)
import Data.Text (pack)
import qualified Logic.Logic as Logic
import Logic.Parse.Internal ( _parseJSONo, _mwithArrayItem, _withArraymItem )
import Text.Read (readEither)
import VK.Update ( Init, Attachment(..), Entity(Entity), Update )

_parseUpdateId :: Object -> Parser (Maybe UpdateId)
_parseUpdateId o = do
  mstr <- o .:? "ts"
  case mstr of
    Nothing -> return Nothing
    Just str -> do
      let euid = readEither str
      case euid of
        Left e -> fail (e ++ ": " ++ str)
        Right uid -> return $ Just uid

_parseInit :: Object -> Parser Init
_parseInit o = do
  response <- o .: "response"
  _parseJSONo response

_parseUpdates :: Object -> Parser [Update]
_parseUpdates = _withArraymItem "updates" _parseUpdate

_parseUpdate :: Object -> Parser (Maybe Update)
_parseUpdate o = do
  _type <- o .: "type" :: Parser String
  case _type of
    "message_new" -> do
      obj <- o .: "object"
      userId <- obj .: "user_id" :: Parser UserId
      text <- obj .: "body"
      let emc = Logic.toMessageCommand text
      mas <- _mwithArrayItem "attachments" _parseAttachment obj
      let as = fromMaybe [] mas
      return $ Just (userId, Entity emc as)
    _ -> return Nothing

_parseAttachment :: Object -> Parser Attachment
_parseAttachment o = do
  _type <- o .: "type" :: Parser String
  case _type of
    "sticker" -> _parseSticker o
    "audio" -> _parseAudio o
    "photo" -> _parseAttachmentItem "photo" o
    "video" -> _parseAttachmentItem "video" o
    "doc" -> _parseAttachmentItem "doc" o
    "wall" -> _parseWall o
    "link" -> _parseLink o
    _ -> fail "Unknown attachment"

_parseAttachmentItem :: String -> Object -> Parser Attachment
_parseAttachmentItem str o = do
  item <- o .: pack str
  itemId <- item .: "id"
  ownerId <- item .: "owner_id"
  accessKey <- item .: "access_key"
  return $ Item str ownerId itemId accessKey

_parseSticker :: Object -> Parser Attachment
_parseSticker o = do
  sticker <- o .: "sticker"
  stickerId <- sticker .: "id"
  return . Sticker $ stickerId

_parseAudio :: Object -> Parser Attachment
_parseAudio o = do
  audio <- o .: "audio"
  audioId <- audio .: "id"
  ownerId <- audio .: "owner_id"
  return $ Audio ownerId audioId

_parseWall :: Object -> Parser Attachment
_parseWall o = do
  wall <- o .: "wall"
  wallId <- wall .: "id"
  ownerId <- wall .: "to_id"
  return $ Wall ownerId wallId

_parseLink :: Object -> Parser Attachment
_parseLink o = do
  item <- o .: "link"
  url <- item .: "url"
  return $ Link url