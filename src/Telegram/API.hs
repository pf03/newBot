--importPriority = 11
module Telegram.API where

import qualified App 
import Types --100
import Telegram.Types --99
--import App
import Telegram.Parse --99
import Logic
import Parse
import Data.Maybe

import Network.HTTP.Simple
import Control.Monad.Trans.Except

queryGetUpdates :: Maybe UpdateId -> TimeOut -> Query 
queryGetUpdates moffset timeout = "timeout"<:> timeout ++ "offset" <:?> moffset

querySendMessage :: Update -> [Label] -> Except E (API, Query)
querySendMessage (cid, en) btns = do
  api <- _getAPI en
  query <- case en of 
    Command _ -> throwE $ QueryError "Невозможно послать команду пользователю"  
    _ -> return $ helper en
  return (api, "chat_id" <:> cid ++ query) where
    helper :: Entity -> Query 
    helper en = case en of  
        Message m -> if null btns 
            then "text" <:> m
            else "text" <:> m ++ "reply_markup" <:> keyboard btns
        Sticker fid -> "sticker" <:> fid
        Animation fid -> "animation" <:> fid
        Photo fid mc -> "photo" <:> fid ++ "caption" <:?> mc
        Video fid mc -> "video" <:> fid ++ "caption" <:?> mc
        Document fid mc -> "document" <:> fid ++ "caption" <:?> mc
        Poll _ q os -> "question" <:> q ++ "options" <:> pollOptions os
        Contact pn fn mln mvc -> "phone_number" <:> pn ++ "first_name" <:> fn ++ "last_name" <:?> mln ++ "vcard" <:?> mvc
        Location x y -> "latitude" <:> x ++ "longitude" <:> y
        Forward fcid mid -> "from_chat_id" <:> cid ++ "message_id" <:> mid  --так работает! --не работает с fcid, так как он не понимает тогда mid
        Other mid -> "from_chat_id" <:> cid ++ "message_id" <:> mid  --from_chad_id в общем случае не равен cid?
        --     !!!!!!нужно настроить высший приоритет Forward, а не Message!!!
_getAPI :: Entity -> Except E API 
_getAPI Command {} = throwE $ QueryError "Невозможно послать команду пользователю"
_getAPI en = return $ helper en where
  helper :: Entity -> API
  helper Message {} = SendMessage
  helper Sticker {} = SendSticker
  helper Animation {} = SendAnimation
  helper Photo {} = SendPhoto
  helper Video {} = SendVideo
  helper Document {} = SendDocument
  helper Poll {} = SendPoll
  helper Contact {} = SendContact
  helper Location {} = SendLocation
  helper Forward {} = ForwardMessage
  helper Other {} = CopyMessage
  

