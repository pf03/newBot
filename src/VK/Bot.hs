{-# LANGUAGE MultiParamTypeClasses #-}
--{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
-- {-# LANGUAGE DeriveGeneric #-}

module VK.Bot 
--(App.Main, test, Pointer(..)) 

where

-- Our modules
import qualified VK.Parse as Parse --49
import VK.Query as Query --11
import VK.Update as Update --60
import qualified Logic.Request as Request --10
import Interface.MCache as Cache
import Interface.MError as Error
import Interface.MLog as Log
import Interface.MT
import Interface.Messenger.IBot as Bot --60
import Common.Misc
import VK.API as API
import VK.Parse as Parse
-- import VK.App --31

-- Other Modules
import Control.Monad.State.Lazy
import Control.Monad.Except
import Control.Monad
import Data.List.Split 
import Data.Maybe
import GHC.Generics
import Data.Aeson
import qualified System.Console.ANSI as Color (Color(..)) 

--каждая функция должна соответствовать одному запросу и его обработке

-----------------------------Types---------------------------------------------
data Pointer = Pointer --синглтон для указания на текущее приложение

--orphan instance для Init или Update - newtype
instance IBot Pointer Init Update  where
    getInit pointer = _getInit 
    getUpdateId = ts
    setUpdateId init newts = init {ts = newts}
    getUpdates = _getUpdates
    sendMessage = _sendMessage


--вызывается либо в начале, либо когда произошла ошибка
_getInit :: MT m => m Init
_getInit = do
    Log.setSettings (Color.Blue, True, "getInit") 
    --ConfigApp _name _host token _updateId _  _repeatNumber groupId version <- gets configApp
    ConfigApp _name _host token _updateId _  _repeatNumber groupId version <- Cache.getConfigApp
    let api = API Groups GetLongPollServer 
    Log.sendT
    json <- Request.api api (Query.getLongPollServer token groupId version) False 
    Log.receiveT
    o <- Parse.getObject json
    Log.receiveDataT "object" o
    init@(Init _server _key _ts) <-  Parse.init o 
    Log.receiveDataT "init" init
    return init


--в дальнейшем возможно переделать, чтобы не сохранять config каждый цикл, а только в конце работы бота, поэтому мы его пердаем в getUpdates
_getUpdates :: MT m => Init -> m ([Update], Init)
_getUpdates init@(Init server key ts) = do
    Log.setSettings (Color.Cyan, False, "getUpdates") 
    --let newInit = init {ts=updateId}
    --let newInit = init
    let query = Query.longPoll init 25
    (host, path) <- parseServer server
    let request = Request.build host path query
    Log.sendT
    json <- Request.send request True   --непосредственно long polling
    Log.receiveT
    o <- Parse.getObject json
    Log.receiveDataT "object" o
    muid <- Parse.updateId o
    let newInit = init {ts = fromMaybe ts muid}
    --let newInit = init
    Log.receiveDataT "updateId" muid
    updates <- Parse.updates o
    Log.receiveDataT "updates" updates
    return (updates, newInit)

_sendMessage :: MT m => Update -> [Label] -> m ()
_sendMessage update@(cid, en) btns = do
    --undefined
    Log.setSettings (Color.Yellow, True, "sendMessage") 
    ConfigApp _name _host token _updateId _ _repeatNumber _groupId version <- Cache.getConfigApp
    Log.sendT
    printT btns
    query <- Query.sendMessage token version update btns
    Log.receiveDataT "query" query
    json <- Request.api (API Messages Send) query False 
    Log.receiveT
    o <- Parse.getObject json
    Log.receiveDataT "object" o
    --undefined    

 --"https://lp.vk.com/wh777777777" -> "lp.vk.com" "/wh777777777"
parseServer :: MError m => String -> m (Host, Path)
parseServer server = do
    let error n = QueryError $ template "({1})Could not match pattern \"https://{host}/{path}\" in long polling server \"{0}\"   === " [server, show n]
    let strs = splitOn "//" server
    case strs of
        [https, str] -> do
            if https /= "https:" then Error.throw $ error 1 else do
                let strs1 = splitOn "/" str
                case strs1 of
                    [host, path] -> return (host, '/':path)
                    _ -> Error.throw $ error 2
        _ -> Error.throw $ error 3


--приходит ошибка, причем ts Int, а не Str {"failed":1,"ts":589}
_getAllUpdates :: MT m => m ([Update], Init)
_getAllUpdates = do
    init <- _getInit
    let newInit = init{ts=500}
    _getUpdates newInit 

-- upd = runT _getAllUpdates

_test::MT m => m()
_test = do
    -- ConfigApp _name _host token _updateId _ _repeatNumber _groupId version <- gets configApp
    ConfigApp _name _host token _updateId _ _repeatNumber _groupId version <- Cache.getConfigApp
    Log.setSettings (Color.Yellow, True, "testRequest") 
    Log.sendT
    let query = Query.test token version
    Log.receiveDataT "query" query
    json <- Request.api (API Messages Send) query False 
    Log.receiveT 
    o <- Parse.getObject json
    Log.receiveDataT "object" o

-- test = runT _test


