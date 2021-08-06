module Logic.Telegram.Query where

import Common.Convert ((<:>), (<:?>))
import Common.Types (FileId (FileId), Label, TimeOut, UpdateId)
import qualified Interface.Error.Exports as Error
import qualified Logic.Telegram.Encode as Encode
import qualified Messenger.Update.Telegram.Types as Update
import Network.HTTP.Simple (Query)
import Control.Exception
import Messenger.Bot.Telegram.Types( API(..) )

getUpdatesQuery :: Maybe UpdateId -> TimeOut -> Query
getUpdatesQuery mOffset timeout = "timeout" <:> timeout ++ "offset" <:?> mOffset

sendMessageQuery :: Update.Update -> [Label] -> Either Error.Error (API, Query)
sendMessageQuery (chatId, entity) btns = do
  api <- getAPI entity
  query <- case entity of
    Update.Message message ->
      return $
        if null btns
          then "text" <:> message
          else "text" <:> message ++ "reply_markup" <:> Encode.encodeKeyboard btns
    Update.Command _ -> Left $ Error.QueryError "Unable to send command to user"
    Update.Sticker (FileId fileId) -> return $ "sticker" <:> fileId
    Update.Animation fileId -> return $ "animation" <:> fileId
    Update.Photo fileId mCaption -> return $ "photo" <:> fileId ++ "caption" <:?> mCaption
    Update.Video fileId mCaption -> return $ "video" <:> fileId ++ "caption" <:?> mCaption
    Update.Document fileId mCaption -> return $ "document" <:> fileId ++ "caption" <:?> mCaption
    Update.Poll _ question options -> return $ "question" <:> question ++ "options" <:> Encode.encodePollOptions options
    Update.Contact phoneNumber firstName mLastName mVCard ->
      return $
        "phone_number" <:> phoneNumber
          ++ "first_name" <:> firstName
          ++ "last_name" <:?> mLastName
          ++ "vcard" <:?> mVCard
    Update.Location x y -> return $ "latitude" <:> x ++ "longitude" <:> y
    Update.Forward _ messageId -> return $ "from_chat_id" <:> chatId ++ "message_id" <:> messageId
    Update.Other messageId -> return $ "from_chat_id" <:> chatId ++ "message_id" <:> messageId
  return (api, "chat_id" <:> chatId ++ query)
  where
    getAPI :: Update.Entity -> Either Error.Error API
    getAPI Update.Message {} = return SendMessage
    getAPI Update.Command {} = Left $ Error.QueryError "Unable to send command to user"
    getAPI Update.Sticker {} = return SendSticker
    getAPI Update.Animation {} = return SendAnimation
    getAPI Update.Photo {} = return SendPhoto
    getAPI Update.Video {} = return SendVideo
    getAPI Update.Document {} = return SendDocument
    getAPI Update.Poll {} = return SendPoll
    getAPI Update.Contact {} = return SendContact
    getAPI Update.Location {} = return SendLocation
    getAPI Update.Forward {} = return ForwardMessage
    getAPI Update.Other {} = return CopyMessage