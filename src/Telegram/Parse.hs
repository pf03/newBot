--{-# LANGUAGE TupleSections #-}
{-# LANGUAGE OverloadedStrings #-}
--{-# LANGUAGE TypeApplications #-}
--importPriority = 49
module Telegram.Parse (module Parse, Telegram.Parse.updateId, updates, keyboard, pollOptions) where
--import qualified Data.ByteString as B
--import qualified Data.ByteString.Char8 as BC
--import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString.Lazy.Char8 as LC

import Data.Aeson
import Data.Aeson.Types
import Data.Text (Text, pack)
--import GHC.Generics

import GHC.Exts -- (fromList)

--наши модули
import Interface.Error as Error --70
import Types --100
import Telegram.Types  --99
import Parse  --50
import Logic --30 !! направление импорта!! --использовать некие Common ФУНКЦИИ


import Control.Monad.Except
import Control.Monad.Trans.Except
import Data.Maybe
import Control.Applicative

-----------------------------Types---------------------------------------------
type OResultItem = Object
type OMessageItem = Object

-----------------RECEIVE-----------------------------



------------------External functions------------------------
updateId :: Object -> Except E (Maybe UpdateId)
updateId o = do
  ids <- _parseE _parseUpdateIds o
  case ids of
    [] -> return Nothing --throwError $ ParseError "Обновления не обнаружены"
    x:xs -> return $ Just $ maximum ids

-- parseChatMessage :: Object -> Except  E Update
-- parseChatMessage = _parseE _parseChatMessage

updates :: Object -> Except  E [Update]
updates = _parseE _parseUpdates

--------------------Internal functions--------------------------

_parseUpdateIds :: Object -> Parser [UpdateId]
_parseUpdateIds = _withArrayItem "result" (.: "update_id") 

_parseChatLastMessage :: Object -> Parser Update
_parseChatLastMessage o = do
  cm <- _parseUpdates o
  case cm of
    [] -> fail "No message to reply"
    x:xs -> return $ last xs

_parseUpdates :: Object -> Parser [Update]
_parseUpdates = _withArraymItem "result" _parseUpdate

_parseUpdate :: OResultItem -> Parser (Maybe Update)
_parseUpdate o = do
      mmessage <- o.:?"message"  --если сообщения нет, то игнорируем такое обновление, это например редактирование сообщения или голос в опросе
      case mmessage of 
        Nothing -> return Nothing
        Just message -> do
          chat <- message.:"chat" --update будет иметь тип [Maybe ChatId, Maybe Entity] - надо подумать
          chatId <- chat.:"id"
          mforward <- _parseForward message 
          case mforward of 
            Just forward -> return . Just $ (chatId, forward)
            Nothing -> do
              mtext <- message .:? "text"
              case mtext of 
                Nothing -> do 
                  mother <- _parseOther message --всегда Just
                  msticker <- _parseSticker message 
                  manimation <- _parseAnimation message
                  mphoto <- _parsePhoto message
                  mvideo <- _parseVideo message
                  mdocument <- _parseDocument message
                  mpoll <- _parsePoll message
                  mcontact <- _parseContact message
                  mlocation <- _parseLocation message
                  --приоритет mother определяется его положением в строке
                  let mentity = mforward <|> mother <|> msticker <|> manimation <|> mphoto <|> mvideo <|> mdocument <|> mpoll <|> mcontact <|> mlocation
                  case mentity of
                    Nothing -> fail "Unknown entity type"
                    Just entity -> return . Just $ (chatId, entity)
                Just text ->  do --команда боту или просто текст или forward
                  let emc = toMessageCommand text
                  case emc of 
                    Left message -> return . Just $  (chatId, Message message)
                    Right command -> do  --проверяем, что это команда
                      typeEntities <- _withArrayItem "entities" (.: "type") message
                      if ("bot_command"::String) `elem` typeEntities
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
      --userName <- forwardFrom .: "username"
      return $ Just $ Forward forwardFromId messageId --userName

--всегда Just
_parseOther :: OMessageItem -> Parser (Maybe Entity)  
_parseOther o = do
    messageId <- o.:"message_id"
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
  mphotos <- _mwithArrayItem "photo" (.:"file_id") message 
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
      question <- o .: "question"
      options <- _withArrayItem "options" (.:"text") o
      return $ Poll intId question options

_parseContact :: OMessageItem -> Parser (Maybe Entity)     
_parseContact  = _mwithItem "contact" $ \o -> do
      phoneNumber <- o .: "phone_number"
      firstName <- o .: "first_name"
      lastName <- o .:? "last_name"
      vсard <- o .:? "vcard"
      return $ Contact phoneNumber firstName lastName vсard

_parseLocation :: OMessageItem -> Parser (Maybe Entity)     
_parseLocation  = _mwithItem "location" $ \o -> do
      latitude <- o .: "latitude"
      longitude <- o .: "longitude"
      return $ Location latitude longitude




-------------SEND---------------------
--кнопки в формате json
--Data Keyboard = Keyboard{}
keyboard :: [String] -> LC.ByteString 
--keyboard :: [String] -> Value
keyboard strs =   encode $  object [ "keyboard" .=  Array ( fromList [Array $ fromList (arr strs)] ) ]
  where
    arr:: [String] -> [Value]
    arr = map (\str -> object["text" .= str])  

pollOptions :: [String] -> LC.ByteString 
pollOptions options = encode $ Array $ fromList (map (String . pack)  options)
