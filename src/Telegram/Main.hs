{-# LANGUAGE MultiParamTypeClasses #-}
--{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}

--importPriority = 1
module Telegram.Main where

--общие функции 
import Types --100
import Telegram.Types --99
import Transformer --20
import API --10
import Telegram.API  --11
import Telegram.Logic --31
import Parse --50
import Config --40
import Logic --30
import Color
import qualified App --60
import qualified Log
import Common
import Class
import qualified State as S


import System.Console.ANSI

--только специфические для Telegram функции 
import Telegram.Parse

--внешние модули
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
    Log.setSettings (Blue, True, "_getUpdateId") 
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
    Log.setSettings (Cyan, True, template "_getUpdates, muid = {0}" [show muid]) 
    Log.sendT
    response <- apiRequest GetUpdates (queryGetUpdates (fmap (+1) muid) 25) True 
    Log.receiveT
    o <- toT $ getObject response
    Log.receiveDataT "object -- convert" o
    mnewuid <- toT $ parseUpdateId o
    Log.receiveDataT "mnewuid" mnewuid
    us <- toT $ parseChatMessages o
    Log.receiveDataT "update" us
    return (us, mnewuid <|> muid)

--отвечаем одному пользователю Update -> UserId
_sendMessage :: Update -> [Label] -> T ()
_sendMessage update@(cid, en) btns = do
    Log.setSettings (Yellow, True, "sendMessage") 
    Log.sendT
    --printT en
    (api, query) <- toT $ querySendMessage update btns
    Log.receiveDataT "(api, query)" (api, query) 
    json <- apiRequest api query False
    Log.receiveT
    o <- toT $ getObject json
    Log.receiveDataT "object" o

--сброс сообщений, которые мы не можем распарсить
reset :: T ()
reset = do
    uid <- S.getUpdateId
    Log.setSettings (Cyan, True, template "reset, uid = {0}" [show uid]) 
    Log.sendT
    response <- apiRequest GetUpdates (queryGetUpdates (Just uid) 0) True 
    Log.receiveT
    o <- toT $ getObject response 
    mnewuid <- toT $ parseUpdateId o
    undefined
    --   ifJust mnewuid do
    --     let newuid = fromJust mnewuid + 1
    --     putStrLnT $ template "Отправляем новый запрос  c uid = {0} ........" [show newuid]
    --     newResponse <- apiRequest GetUpdates (queryGetUpdates newuid 0) True 
    --     printT newResponse

res :: IO ()
res = runT reset


