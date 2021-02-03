{-# LANGUAGE MultiParamTypeClasses #-}
--{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}

--importPriority = 1
module VK.Main (App.Main, test, Pointer(..)) where
import Types hiding (repeat)--100 
import VK.Types --99
import Transformer --20
import qualified VK.Query as Query --11
import qualified Request --10
import qualified VK.Parse as Parse --49
import Log
import Class
import qualified App --60
import Common
import VK.App --31

import Control.Monad.State.Lazy
import Control.Monad.Except
import Control.Monad
import Data.List.Split 
import Data.Maybe

import qualified System.Console.ANSI as Color (Color(..)) 

--каждая функция должна соответствовать одному запросу и его обработке

instance App.Main Pointer Init Update  where
    getInit pointer = _getInit 
    getUpdateId = ts
    setUpdateId init newts = init {ts = newts}
    getUpdates = _getUpdates
    sendMessage = _sendMessage


--вызывается либо в начале, либо когда произошла ошибка
_getInit :: T Init
_getInit = do
    Log.setSettings (Color.Blue, True, "getInit") 
    ConfigApp _name _host token _updateId _  _repeatNumber groupId version <- gets configApp
    let api = API Groups GetLongPollServer 
    Log.sendT
    json <- Request.api api (Query.getLongPollServer token groupId version) False 
    Log.receiveT
    o <- toT $ Parse.getObject json
    Log.receiveDataT "object" o
    init@(Init _server _key _ts) <- toT $ Parse.init o 
    Log.receiveDataT"init" init
    return init


--в дальнейшем возможно переделать, чтобы не сохранять config каждый цикл, а только в конце работы бота, поэтому мы его пердаем в getUpdates
_getUpdates :: Init -> T ([Update], Init)
_getUpdates init@(Init server key ts) = do
    Log.setSettings (Color.Cyan, False, "getUpdates") 
    --let newInit = init {ts=updateId}
    --let newInit = init
    let query = Query.longPoll init 25
    (host, path) <- toT $ parseServer server
    let request = Request.build host path query
    Log.sendT
    json <- toT $ Request.send request True   --непосредственно long polling
    Log.receiveT
    o <- toT $ Parse.getObject json
    Log.receiveDataT "object" o
    muid <- toT $ Parse.updateId o
    let newInit = init {ts = fromMaybe ts muid}
    --let newInit = init
    Log.receiveDataT "updateId" muid
    updates <- toT $ Parse.updates o
    Log.receiveDataT "updates" updates
    return (updates, newInit)

_sendMessage :: Update -> [Label] -> T ()
_sendMessage update@(cid, en) btns = do
    --undefined
    Log.setSettings (Color.Yellow, True, "sendMessage") 
    ConfigApp _name _host token _updateId _ _repeatNumber _groupId version <- gets configApp
    Log.sendT
    printT btns
    query <- toT $ Query.sendMessage token version update btns
    Log.receiveDataT "query" query
    json <- Request.api (API Messages Send) query False 
    Log.receiveT
    o <- toT $ Parse.getObject json
    Log.receiveDataT "object" o
    --undefined    

 --"https://lp.vk.com/wh777777777" -> "lp.vk.com" "/wh777777777"
parseServer :: String -> Except E (Host, Path)
parseServer server = do
    let error n = QueryError $ template "({1})Could not match pattern \"https://{host}/{path}\" in long polling server \"{0}\"   === " [server, show n]
    let strs = splitOn "//" server
    case strs of
        [https, str] -> do
            if https /= "https:" then throwError $ error 1 else do
                let strs1 = splitOn "/" str
                case strs1 of
                    [host, path] -> return (host, '/':path)
                    _ -> throwError $ error 2
        _ -> throwError $ error 3


--приходит ошибка, причем ts Int, а не Str {"failed":1,"ts":589}
_getAllUpdates :: T ([Update], Init)
_getAllUpdates = do
    init <- _getInit
    let newInit = init{ts=500}
    _getUpdates newInit 

upd = runT _getAllUpdates

_test::T()
_test = do
    ConfigApp _name _host token _updateId _ _repeatNumber _groupId version <- gets configApp
    Log.setSettings (Color.Yellow, True, "testRequest") 
    Log.sendT
    let query = Query.test token version
    Log.receiveDataT "query" query
    json <- Request.api (API Messages Send) query False 
    Log.receiveT 
    o <- toT $ Parse.getObject json
    Log.receiveDataT "object" o

test = runT _test


