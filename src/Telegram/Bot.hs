{-# LANGUAGE MultiParamTypeClasses #-}
--{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances     #-}
module Telegram.Bot 
--(App.Main, reset, Pointer(..)) 
where

-- Our modules
import           Common.Misc
import           Interface.MLog           as Log
import           Interface.MT
import           Interface.MCache as Cache
import           Interface.Messenger.IBot
import qualified Logic.Request as Request
import qualified Telegram.Parse           as Parse
import qualified Telegram.Query           as Query
import           Telegram.Update           as Update
import           Telegram.API           as API

--Other modules
import           Control.Applicative
import           Control.Monad.State.Lazy
import           Data.Maybe
import qualified System.Console.ANSI      as Color (Color (..))

-- instance App.Auth Auth where
--     --getAuth1 ::  T Auth
--     getAuth1 = return $ Auth ()

data Pointer = Pointer 

instance IBot Pointer UpdateId Update where
    getInit pointer = _getUpdateId
    getUpdateId = id
    setUpdateId uid newuid = newuid
    getUpdates uid = do
       (us, Just uid) <- _getUpdates . Just $ uid
       return (us, uid)
    sendMessage = _sendMessage


--------------------------------Internal functions----------------------------------------
_getUpdateId :: MT m => m UpdateId
_getUpdateId = do
    Log.setSettings Color.Blue True "_getUpdateId"
    updateIdFromFile <- Cache.getUpdateIdFromFile
    if updateIdFromFile
        then  return 0  --если updateId получаем из файла, то инициализация не нужна
        else do
            Log.send
            (_, muid) <- _getUpdates Nothing
            Log.receive
            maybe _getUpdateId (return . (-) 1 ) muid --отнимаем единицу, чтобы второй запрос был с muid, а не muid+1

--по умолчанию возвращается то же значение, что и было
--наша функция более универсальная, чем требует интерфейс, а именно использует Maybe UpdateId вместо UpdateId
--поэтому она может использоваться и для инициализации, когда UpdateId нету, и для основной работы
_getUpdates :: MT m => Maybe UpdateId -> m ([Update], Maybe UpdateId)
_getUpdates muid = do
    Log.setSettings Color.Cyan True $ template "_getUpdates, muid = {0}" [show muid]
    Log.send
    response <- Request.api GetUpdates (Query.getUpdates (fmap (+1) muid) 25) True
    Log.receive
    o <- Parse.getObject response
    Log.receiveData "object -- convert" o
    mnewuid <- Parse.updateId o
    Log.receiveData "mnewuid" mnewuid
    us <- Parse.updates o
    Log.receiveData "update" us
    return (us, mnewuid <|> muid)

--отвечаем одному пользователю Update -> UserId
_sendMessage :: MT m => Update -> [Label] -> m ()
_sendMessage update@(cid, en) btns = do
    Log.setSettings Color.Yellow True "sendMessage"
    Log.send
    (api, query) <- Query.sendMessage update btns
    Log.receiveData "(api, query)" (api, query)
    json <- Request.api api query False
    Log.receive
    o <- Parse.getObject json
    Log.receiveData "object" o

--сброс сообщений, которые мы не можем распарсить
_reset :: MT m => m ()
_reset = do
    uid <- Cache.getUpdateId
    Log.setSettings Color.Cyan True $ template "reset, uid = {0}" [show uid]
    Log.send
    json <- Request.api GetUpdates (Query.getUpdates (Just uid) 0) True
    Log.receive
    o <- Parse.getObject json
    mnewuid <- Parse.updateId o
    Log.receiveData "mnewuid" mnewuid

-- reset :: IO ()
-- reset = runT reset


