{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE DerivingStrategies #-}

module VK.Bot (Pointer(..)) where

-- Our modules
import           Common.Misc
import           Interface.MCache         as Cache hiding (host)
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
import           Control.Concurrent

-----------------------------Types---------------------------------------------
data Pointer = Pointer
-- New type wrappers in order to avoid orphan instances
newtype WrapInit = WrapInit Init
newtype WrapUpdate = WrapUpdate Update deriving newtype (IUpdate)

-----------------------------Instance------------------------------------------
instance IBot Pointer WrapInit WrapUpdate  where
    getInit :: MT m => Pointer -> MVar Bool -> m WrapInit
    getInit _ _ = WrapInit <$> _getInit

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
    ConfigApp _name _host tk _updateId _  _repeatNumber gid v <- Cache.getConfigApp
    let api = API Groups GetLongPollServer
    Log.send
    json <- Request.api api (Query.getLongPollServer tk gid v) False
    Log.receive
    o <- Parse.getObject json
    Log.receiveData "object" o
    ini@(Init _server _key _ts) <-  Parse.init o
    Log.receiveData "init" ini
    return ini

-- Get updates from messenger server by the long polling method
_getUpdates :: MT m => Init -> m ([Update], Init)
_getUpdates ini@(Init server0 _ ts0) = do
    Log.setSettings Color.Cyan False "getUpdates"
    let query = Query.longPoll ini 25
    (host, path) <- parseServer server0
    let request = Request.build host path query
    Log.send
    json <- Request.send request True -- long polling
    Log.receive
    o <- Parse.getObject json
    Log.receiveData "object" o
    muid <- Parse.updateId o
    let newIni = ini {ts = fromMaybe ts0 muid}
    Log.receiveData "updateId" muid
    us <- Parse.updates o
    Log.receiveData "updates" us
    return (us, newIni)

-- Send response to a single user
_sendMessage :: MT m => Update -> [Label] -> m ()
_sendMessage update btns = do
    --undefined
    Log.setSettings Color.Yellow True "sendMessage"
    ConfigApp _name _host tk _updateId _ _repeatNumber _groupId v <- Cache.getConfigApp
    Log.send
    printT btns
    query <- Query.sendMessage tk v update btns
    Log.receiveData "query" query
    json <- Request.api (API Messages Send) query False
    Log.receive
    o <- Parse.getObject json
    Log.receiveData "object" o
    --undefined

 -- "https://lp.vk.com/wh777777777" -> "lp.vk.com" "/wh777777777"
parseServer :: MError m => String -> m (Host, Path)
parseServer url = do
    let err = QueryError $ 
            template "Could not match pattern \"https://{host}/{path}\" in long polling server \"{0}\"" [url]
    let strs = splitOn "//" url
    case strs of
        [https, str] -> do
            if https /= "https:" then Error.throw err else do
                let strs1 = splitOn "/" str
                case strs1 of
                    [host, path] -> return (host, '/':path)
                    _            -> Error.throw err
        _ -> Error.throw err