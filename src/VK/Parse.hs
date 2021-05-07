{-# LANGUAGE OverloadedStrings #-}

--importPriority = 49
module VK.Parse 
--(module Parse, VK.Parse.init, VK.Parse.updateId, updates, keyboard, contentUrl, contentMessage) 
where

-- Our modules
import Interface.Error
import Common.Misc
-- import Types --100
-- import VK.Types --99
import Logic.Parse --50
import Logic.Logic as Logic
import VK.App 

-- Other modules
--import qualified Data.ByteString as B
--import qualified Data.ByteString.Char8 as BC
--import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString.Lazy.Char8 as LC
import Data.Aeson
import Data.Aeson.Types
import Data.Text (Text, pack)
--import GHC.Generics
import GHC.Exts -- (fromList)
import Control.Monad.Except
import Control.Monad.Trans.Except
import Data.Maybe
import Control.Applicative
import Text.Read


-----------------------------Types---------------------------------------------


-----------------------RECEIVE----------------------------------------

------------------External functions---------------------------

init :: MError m => Object -> m Init
init = _parseE _parseInit

updateId :: MError m => Object -> m (Maybe UpdateId)
updateId = _parseE _parseUpdateId

-- chatMessage :: Object -> Except  E Update
-- chatMessage = _parseE _parseChatMessage

updates :: MError m => Object -> m [Update]
updates = _parseE _parseUpdates

--------------------Internal functions--------------------------
_parseUpdateId :: Object -> Parser (Maybe UpdateId)
_parseUpdateId o = do
    mstr <- o.:?"ts"
    case mstr of
        Nothing -> return Nothing 
        Just str -> do
            let euid = readEither str -- :: Either String UpdateId
            case euid of
                Left e -> fail (e ++ ": " ++ str)
                Right uid -> return $ Just uid 

_parseInit :: Object -> Parser Init
_parseInit o = do
    response <- o.:"response" 
    _parseJSONo response

_parseUpdates :: Object -> Parser [Update]
_parseUpdates = _withArraymItem "updates" _parseUpdate

_parseUpdate :: Object -> Parser (Maybe Update)
_parseUpdate o = do
    _type <- o.:"type" :: Parser String 
    case _type of 
        "message_new" -> do 
            object <-  o.: "object"
            userId <- object .: "user_id"::Parser UserId
            text <- object .: "body"
            let emc = Logic.toMessageCommand text
            --attachments <- object .: "attachments"
            mattachments <-_mwithArrayItem "attachments" _parseAttachment object
            let attachments = fromMaybe [] mattachments 
            return $ Just (userId, Entity emc attachments)
        _ -> return Nothing

_parseAttachment :: Object -> Parser Attachment
_parseAttachment o = do
    _type <- o .: "type"::Parser String 
    case _type of
        "sticker" -> _parseSticker o
        -- "photo" -> _parsePhoto o
        -- "video" -> _parseVideo o
        "audio" -> _parseAudio o
        "photo" -> _parseAttachmentItem "photo" o
        "video" -> _parseAttachmentItem "video" o
        "doc" -> _parseAttachmentItem "doc" o
        "wall" -> _parseWall o
        "link" -> _parseLink o
        --str -> _getConstructor str <$> _parseAttachmentItem str o
        _ -> undefined

_parseAttachmentItem :: String -> Object -> Parser Attachment
_parseAttachmentItem str o = do
    item <- o.:pack str
    itemId <- item.: "id"
    ownerId <- item.:"owner_id"
    accessKey <- item.:"access_key"
    return $ Item str ownerId itemId accessKey

_parseSticker :: Object -> Parser Attachment
_parseSticker o = do
    sticker <- o.:"sticker"
    stickerId <- sticker.: "id"
    return . Sticker $ stickerId

_parseAudio :: Object -> Parser Attachment
_parseAudio o = do
    audio <- o.:"audio"
    audioId <- audio.: "id"
    ownerId <- audio.:"owner_id"
    --accessKey <- video.:"access_key"
    return $ Audio ownerId audioId

_parseWall :: Object -> Parser Attachment
_parseWall o = do
    wall <- o.:"wall"
    wallId <- wall.: "id"
    ownerId <- wall.:"to_id"
    --accessKey <- video.:"access_key"
    return $ Wall ownerId wallId

_parseLink :: Object -> Parser Attachment
_parseLink o = do
    item <- o.:"link"
    url <- item.: "url"
    return $ Link url





------------------------SEND----------------------------------------

-------------SEND---------------------
--кнопки в формате json
keyboard :: [String] -> LC.ByteString 
keyboard strs = encode $  object [ 
    "one_time" .= True,
    "inline" .= False,
    "buttons" .= Array ( fromList [Array $ fromList (_buttons strs)] ) ]

_buttons :: [String] -> [Value]
_buttons = map $ \str -> object ["action".=object[
        "type".=("text"::String),
        "payload".=("{}"::String),
        "label".=str
        ]
    ] 

contentUrl:: String -> LC.ByteString 
contentUrl str = encode $  object [ 
    "type" .= ("url"::String),
    "url" .= str
    ]

contentMessage:: OwnerId -> GroupId -> IntId -> LC.ByteString 
contentMessage ownerId peerId messageId = encode $  object [ 
    "type" .= ("message"::String),
    "owner_id" .= ownerId,
    "peer_id" .= peerId,
    "conversation_message_id" .= messageId
    ]

-- {
--     type: 'message',
--     owner_id: 0000, // от чьего имени указан peer_id. т.е. вы можете использовать контент из сообщения другой группы.
--     peer_id: 0000, // id диалога
--     conversation_message_id: 0000, // id сообщения в беседе. Не путать с message.id профиля
-- }

--     {
--     type: "url",
--     url: "https://m.vk.com/poll-201551107_494468876"
-- }