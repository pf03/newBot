{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module VK.Parse
  ( module Logic.Parse,
    Init (..),
    VK.Parse.init,
    VK.Parse.updateId,
    updates,
    keyboard,
    contentUrl,
    contentMessage,
  )
where

import Common.Misc (IntId, LBS, UpdateId, UserId)
import Data.Aeson
  ( FromJSON,
    KeyValue ((.=)),
    Object,
    ToJSON,
    Value (Array),
    encode,
    object,
    (.:),
    (.:?),
  )
import Data.Aeson.Types (Parser)
import Data.Maybe (fromMaybe)
import Data.Text (pack)
import GHC.Exts (IsList (fromList))
import GHC.Generics (Generic)
import Interface.MError.Exports (MError)
import qualified Logic.Logic as Logic
import Logic.Parse
  ( eDecode,
    getObject,
    getValue,
    (<:>),
    (<:?>),
    _mwithArrayItem,
    _mwithItem,
    _parseE,
    _parseJSONo,
    _withArrayItem,
    _withArraymItem,
  )
import Text.Read (readEither)
import VK.Update (Attachment (..), Entity (Entity), GroupId, OwnerId, Update)

-----------------------------Types---------------------------------------------
data Init = Init {server :: String, key :: String, ts :: Int} deriving (Show, Generic)

instance FromJSON Init

instance ToJSON Init

-----------------------------Receive-------------------------------------------
-----------------------------External------------------------------------------

init :: MError m => Object -> m Init
init = _parseE _parseInit

updateId :: MError m => Object -> m (Maybe UpdateId)
updateId = _parseE _parseUpdateId

updates :: MError m => Object -> m [Update]
updates = _parseE _parseUpdates

-----------------------------Internal------------------------------------------
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

-----------------------------Send----------------------------------------------
keyboard :: [String] -> LBS
keyboard strs =
  encode $
    object
      [ "one_time" .= True,
        "inline" .= False,
        "buttons" .= Array (fromList [Array $ fromList (_buttons strs)])
      ]

_buttons :: [String] -> [Value]
_buttons = map $ \str ->
  object
    [ "action"
        .= object
          [ "type" .= ("text" :: String),
            "payload" .= ("{}" :: String),
            "label" .= str
          ]
    ]
contentUrl :: String -> LBS
contentUrl str =
  encode $
    object
      [ "type" .= ("url" :: String),
        "url" .= str
      ]

contentMessage :: OwnerId -> GroupId -> IntId -> LBS
contentMessage ownerId peerId messageId =
  encode $
    object
      [ "type" .= ("message" :: String),
        "owner_id" .= ownerId,
        "peer_id" .= peerId,
        "conversation_message_id" .= messageId
      ]
