{-# LANGUAGE MultiParamTypeClasses #-}
--{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}

--importPriority = 1
module Telegram.Main (App.Main, reset, Pointer(..)) where

--общие функции 
import Types --100
import Telegram.Types --99
import Transformer --20
import qualified Request --10
import qualified Telegram.Query as Query  --11
import qualified App --60
import qualified Log
import Common
import Class
import qualified State as S
import qualified System.Console.ANSI as Color (Color(..)) 
import Telegram.App --31

import qualified Telegram.Parse as Parse

import Control.Monad.State.Lazy
import Data.Maybe
import Control.Applicative

-- instance App.Auth Auth where 
--     --getAuth1 ::  T Auth
--     getAuth1 = return $ Auth ()

instance App.Main Pointer UpdateId Update where
    getInit pointer = _getUpdateId
    getUpdateId = id
    setUpdateId uid newuid = newuid 
    getUpdates uid = do
       (us, Just uid) <- _getUpdates . Just $ uid
       return (us, uid)
    sendMessage = _sendMessage


--------------------------------Internal functions----------------------------------------
_getUpdateId :: T UpdateId
_getUpdateId = do
    Log.setSettings (Color.Blue, True, "_getUpdateId") 
    updateIdFromFile <- S.getUpdateIdFromFile
    if updateIdFromFile
        then  return 0  --если updateId получаем из файла, то инициализация не нужна
        else do
            Log.sendT
            (_, muid) <- _getUpdates Nothing
            Log.receiveT
            maybe _getUpdateId (return . (-) 1 ) muid --отнимаем единицу, чтобы второй запрос был с muid, а не muid+1

--по умолчанию возвращается то же значение, что и было 
--наша функция более универсальная, чем требует интерфейс, а именно использует Maybe UpdateId вместо UpdateId
--поэтому она может использоваться и для инициализации, когда UpdateId нету, и для основной работы
_getUpdates :: Maybe UpdateId -> T ([Update], Maybe UpdateId)
_getUpdates muid = do
    Log.setSettings (Color.Cyan, True, template "_getUpdates, muid = {0}" [show muid]) 
    Log.sendT
    response <- Request.api GetUpdates (Query.getUpdates (fmap (+1) muid) 25) True 
    Log.receiveT
    o <- toT $ Parse.getObject response
    Log.receiveDataT "object -- convert" o
    mnewuid <- toT $ Parse.updateId o
    Log.receiveDataT "mnewuid" mnewuid
    us <- toT $ Parse.updates o
    Log.receiveDataT "update" us
    return (us, mnewuid <|> muid)

--отвечаем одному пользователю Update -> UserId
_sendMessage :: Update -> [Label] -> T ()
_sendMessage update@(cid, en) btns = do
    Log.setSettings (Color.Yellow, True, "sendMessage") 
    Log.sendT
    (api, query) <- toT $ Query.sendMessage update btns
    Log.receiveDataT "(api, query)" (api, query) 
    json <- Request.api api query False
    Log.receiveT
    o <- toT $ Parse.getObject json
    Log.receiveDataT "object" o

--сброс сообщений, которые мы не можем распарсить
_reset :: T ()
_reset = do
    uid <- S.getUpdateId
    Log.setSettings (Color.Cyan, True, template "reset, uid = {0}" [show uid]) 
    Log.sendT
    json <- Request.api GetUpdates (Query.getUpdates (Just uid) 0) True 
    Log.receiveT
    o <- toT $ Parse.getObject json 
    mnewuid <- toT $ Parse.updateId o
    Log.receiveDataT "mnewuid" mnewuid
    
reset :: IO ()
reset = runT reset


