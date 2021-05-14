module Telegram.Query (getUpdates, sendMessage) where

import Common.Misc (Label, TimeOut, UpdateId)
import Interface.MError (E (QueryError), MError)
import qualified Interface.MError as Error (throw)
import Network.HTTP.Simple (Query)
import Telegram.API (API (..))
import Telegram.Parse ((<:>), (<:?>))
import qualified Telegram.Parse as Encode (keyboard, pollOptions)
import Telegram.Update (Entity (..), Update)

getUpdates :: Maybe UpdateId -> TimeOut -> Query
getUpdates moffset timeout = "timeout" <:> timeout ++ "offset" <:?> moffset

sendMessage :: MError m => Update -> [Label] -> m (API, Query)
sendMessage (cid, en) btns = do
  api <- _getAPI en
  -- query <- helper en

  qu <- case en of
    Message m ->
      return $
        if null btns
          then "text" <:> m
          else "text" <:> m ++ "reply_markup" <:> Encode.keyboard btns
    Command _ -> Error.throw $ QueryError "Unable to send command to user"
    Sticker fid -> return $ "sticker" <:> fid
    Animation fid -> return $ "animation" <:> fid
    Photo fid mc -> return $ "photo" <:> fid ++ "caption" <:?> mc
    Video fid mc -> return $ "video" <:> fid ++ "caption" <:?> mc
    Document fid mc -> return $ "document" <:> fid ++ "caption" <:?> mc
    Poll _ q os -> return $ "question" <:> q ++ "options" <:> Encode.pollOptions os
    Contact pn fn mln mvc ->
      return $
        "phone_number" <:> pn
          ++ "first_name" <:> fn
          ++ "last_name" <:?> mln
          ++ "vcard" <:?> mvc
    Location x y -> return $ "latitude" <:> x ++ "longitude" <:> y
    Forward _ mid -> return $ "from_chat_id" <:> cid ++ "message_id" <:> mid
    Other mid -> return $ "from_chat_id" <:> cid ++ "message_id" <:> mid
  return (api, "chat_id" <:> cid ++ qu)

_getAPI :: MError m => Entity -> m API
_getAPI Message {} = return SendMessage
_getAPI Command {} = Error.throw $ QueryError "Unable to send command to user"
_getAPI Sticker {} = return SendSticker
_getAPI Animation {} = return SendAnimation
_getAPI Photo {} = return SendPhoto
_getAPI Video {} = return SendVideo
_getAPI Document {} = return SendDocument
_getAPI Poll {} = return SendPoll
_getAPI Contact {} = return SendContact
_getAPI Location {} = return SendLocation
_getAPI Forward {} = return ForwardMessage
_getAPI Other {} = return CopyMessage
