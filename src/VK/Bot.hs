{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE DerivingStrategies #-}

module VK.Bot (Pointer(..)) where

-- Our modules
import           Common.Misc
import           Interface.MCache         as Cache
import           Interface.MError         as Error
import           Interface.MLog           as Log
import           Interface.MT
import           Interface.Messenger.IBot as Bot
import qualified Logic.Request            as Request
import           VK.API                   as API
import           VK.Parse                 as Parse
import           VK.Query                 as Query
import           VK.Update                as Update
import           Interface.Messenger.IUpdate

-- Other Modules
import           Data.List.Split
import           Data.Maybe
import qualified System.Console.ANSI      as Color (Color (..))

-----------------------------Types---------------------------------------------
data Pointer = Pointer
-- New type wrappers in order to avoid orphan instances
newtype WrapInit = WrapInit Init
newtype WrapUpdate = WrapUpdate Update deriving newtype (IUpdate)

-----------------------------Instance------------------------------------------
instance IBot Pointer WrapInit WrapUpdate  where
    getInit :: MT m => Pointer -> m WrapInit
    getInit _ = WrapInit <$> _getInit

    getUpdateId :: WrapInit -> UpdateId
    getUpdateId (WrapInit ini) = ts ini

    setUpdateId :: WrapInit -> UpdateId -> WrapInit
    setUpdateId (WrapInit ini) newts = WrapInit $ ini {ts = newts}

    getUpdates:: MT m => WrapInit -> m ([WrapUpdate], WrapInit)
    getUpdates (WrapInit ini) = do
        (us, newini) <- _getUpdates ini
        return (WrapUpdate <$> us, WrapInit newini)

    sendMessage:: MT m => WrapUpdate -> [Label] -> m ()
    sendMessage (WrapUpdate u) = _sendMessage u

--------------------------------Internal functions----------------------------------------
-- Initialization - get last updateId, server name, key for getUpdates request
_getInit :: MT m => m Init
_getInit = do
    Log.setSettings Color.Blue True "getInit"
    ConfigApp _name _host token _updateId _  _repeatNumber groupId version <- Cache.getConfigApp
    let api = API Groups GetLongPollServer
    Log.send
    json <- Request.api api (Query.getLongPollServer token groupId version) False
    Log.receive
    o <- Parse.getObject json
    Log.receiveData "object" o
    init@(Init _server _key _ts) <-  Parse.init o
    Log.receiveData "init" init
    return init

-- Get updates from messenger server by the long polling method
_getUpdates :: MT m => Init -> m ([Update], Init)
_getUpdates init@(Init server key ts) = do
    Log.setSettings Color.Cyan False "getUpdates"
    let query = Query.longPoll init 25
    (host, path) <- parseServer server
    let request = Request.build host path query
    Log.send
    json <- Request.send request True -- long polling
    Log.receive
    o <- Parse.getObject json
    Log.receiveData "object" o
    muid <- Parse.updateId o
    let newInit = init {ts = fromMaybe ts muid}
    --let newInit = init
    Log.receiveData "updateId" muid
    updates <- Parse.updates o
    Log.receiveData "updates" updates
    return (updates, newInit)

_sendMessage :: MT m => Update -> [Label] -> m ()
_sendMessage update@(cid, en) btns = do
    --undefined
    Log.setSettings Color.Yellow True "sendMessage"
    ConfigApp _name _host token _updateId _ _repeatNumber _groupId version <- Cache.getConfigApp
    Log.send
    printT btns
    query <- Query.sendMessage token version update btns
    Log.receiveData "query" query
    json <- Request.api (API Messages Send) query False
    Log.receive
    o <- Parse.getObject json
    Log.receiveData "object" o
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
                    _            -> Error.throw $ error 2
        _ -> Error.throw $ error 3


--приходит ошибка, причем ts Int, а не Str {"failed":1,"ts":589}
_getAllUpdates :: MT m => m ([Update], Init)
_getAllUpdates = do
    init <- _getInit
    let newInit = init{ts=500}
    _getUpdates newInit

