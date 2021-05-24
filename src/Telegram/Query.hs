module Telegram.Query (getUpdates, sendMessage) where

import Common.Types (Label, TimeOut, UpdateId)
import Common.Convert ((<:>), (<:?>))
import Interface.Class (MError)
import qualified Interface.MError.Exports as Error
import Network.HTTP.Simple (Query)
import qualified Telegram.API as API
import qualified Telegram.Encode as Encode
import qualified Telegram.Update as Update

getUpdates :: Maybe UpdateId -> TimeOut -> Query
getUpdates moffset timeout = "timeout" <:> timeout ++ "offset" <:?> moffset

sendMessage :: MError m => Update.Update -> [Label] -> m (API.API, Query)
sendMessage (cid, en) btns = do
  api <- getAPI en
  qu <- case en of
    Update.Message m ->
      return $
        if null btns
          then "text" <:> m
          else "text" <:> m ++ "reply_markup" <:> Encode.keyboard btns
    Update.Command _ -> Error.throw $ Error.QueryError "Unable to send command to user"
    Update.Sticker fid -> return $ "sticker" <:> fid
    Update.Animation fid -> return $ "animation" <:> fid
    Update.Photo fid mc -> return $ "photo" <:> fid ++ "caption" <:?> mc
    Update.Video fid mc -> return $ "video" <:> fid ++ "caption" <:?> mc
    Update.Document fid mc -> return $ "document" <:> fid ++ "caption" <:?> mc
    Update.Poll _ q os -> return $ "question" <:> q ++ "options" <:> Encode.pollOptions os
    Update.Contact pn fn mln mvc ->
      return $
        "phone_number" <:> pn
          ++ "first_name" <:> fn
          ++ "last_name" <:?> mln
          ++ "vcard" <:?> mvc
    Update.Location x y -> return $ "latitude" <:> x ++ "longitude" <:> y
    Update.Forward _ mid -> return $ "from_chat_id" <:> cid ++ "message_id" <:> mid
    Update.Other mid -> return $ "from_chat_id" <:> cid ++ "message_id" <:> mid
  return (api, "chat_id" <:> cid ++ qu)

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