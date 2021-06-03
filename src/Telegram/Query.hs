module Telegram.Query where

import Common.Types (Label, TimeOut, UpdateId)
import Common.Convert ((<:>), (<:?>))
import Interface.Class (MError)
import qualified Interface.MError.Exports as Error
import Network.HTTP.Simple (Query)
import qualified Messenger.API.Telegram.Types as API
import qualified Telegram.Encode as Encode
import qualified Messenger.Update.Telegram.Types as Update

getUpdatesQuery :: Maybe UpdateId -> TimeOut -> Query
getUpdatesQuery moffset timeout = "timeout" <:> timeout ++ "offset" <:?> moffset

sendMessageQuery :: MError m => Update.Update -> [Label] -> m (API.API, Query)
sendMessageQuery (chatId, entity) btns = do
  api <- getAPI entity
  query <- case entity of
    Update.Message message ->
      return $
        if null btns
          then "text" <:> message
          else "text" <:> message ++ "reply_markup" <:> Encode.keyboard btns
    Update.Command _ -> Error.throw $ Error.QueryError "Unable to send command to user"
    Update.Sticker fileId -> return $ "sticker" <:> fileId
    Update.Animation fileId -> return $ "animation" <:> fileId
    Update.Photo fileId mcaption -> return $ "photo" <:> fileId ++ "caption" <:?> mcaption
    Update.Video fileId mcaption -> return $ "video" <:> fileId ++ "caption" <:?> mcaption
    Update.Document fileId mcaption -> return $ "document" <:> fileId ++ "caption" <:?> mcaption
    Update.Poll _ question options -> return $ "question" <:> question ++ "options" <:> Encode.pollOptions options
    Update.Contact phoneNumber firstName mlastName mvCard ->
      return $
        "phone_number" <:> phoneNumber
          ++ "first_name" <:> firstName
          ++ "last_name" <:?> mlastName
          ++ "vcard" <:?> mvCard
    Update.Location x y -> return $ "latitude" <:> x ++ "longitude" <:> y
    Update.Forward _ messageId -> return $ "from_chat_id" <:> chatId ++ "message_id" <:> messageId
    Update.Other messageId -> return $ "from_chat_id" <:> chatId ++ "message_id" <:> messageId
  return (api, "chat_id" <:> chatId ++ query)

  where
  getAPI :: MError m => Update.Entity -> m API.API
  getAPI Update.Message {} = return API.SendMessage
  getAPI Update.Command {} = Error.throw $ Error.QueryError "Unable to send command to user"
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