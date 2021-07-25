module Logic.Telegram.Query where

--(Label, TimeOut, UpdateId)

import Common.Convert ((<:>), (<:?>))
import Common.Types (FileId (FileId), Label, TimeOut, UpdateId)
import qualified Interface.Error.Exports as Error
import qualified Logic.Telegram.Encode as Encode
import qualified Messenger.API.Telegram.Types as API
import qualified Messenger.Update.Telegram.Types as Update
import Network.HTTP.Simple (Query)
import Control.Exception

getUpdatesQuery :: Maybe UpdateId -> TimeOut -> Query
getUpdatesQuery mOffset timeout = "timeout" <:> timeout ++ "offset" <:?> mOffset

sendMessageQuery :: Monad m => Update.Update -> [Label] -> m (API.API, Query)
sendMessageQuery (chatId, entity) btns = do
  api <- getAPI entity
  query <- case entity of
    Update.Message message ->
      return $
        if null btns
          then "text" <:> message
          else "text" <:> message ++ "reply_markup" <:> Encode.encodeKeyboard btns
    Update.Command _ -> throw $ Error.QueryError "Unable to send command to user"
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
    getAPI :: Monad m => Update.Entity -> m API.API
    getAPI Update.Message {} = return API.SendMessage
    getAPI Update.Command {} = throw $ Error.QueryError "Unable to send command to user"
    getAPI Update.Sticker {} = return API.SendSticker
    getAPI Update.Animation {} = return API.SendAnimation
    getAPI Update.Photo {} = return API.SendPhoto
    getAPI Update.Video {} = return API.SendVideo
    getAPI Update.Document {} = return API.SendDocument
    getAPI Update.Poll {} = return API.SendPoll
    getAPI Update.Contact {} = return API.SendContact
    getAPI Update.Location {} = return API.SendLocation
    getAPI Update.Forward {} = return API.ForwardMessage
    getAPI Update.Other {} = return API.CopyMessage