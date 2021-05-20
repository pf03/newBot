{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module VK.Bot (Pointer (..)) where

import Common.Misc (Label, Path, UpdateId, printT, template)
import Control.Monad (forM_)
import Data.List.Split (splitOn)
import Data.Maybe (fromMaybe)
import Interface.Class (IBot, IUpdate, MError, MT)
import qualified Interface.MCache.Exports as Cache
import qualified Interface.MError.Exports as Error
import qualified Interface.MLog.Exports as Log
import qualified Interface.Messenger.IBot as IBot
import qualified Interface.Messenger.IUpdate as IUpdate
import qualified Logic.Request as Request
import qualified System.Console.ANSI as Color (Color (..))
import qualified VK.API as API
import qualified VK.Parse as Parse
import qualified VK.Query as Query
import qualified VK.Update as Update

-----------------------------Types---------------------------------------------
data Pointer = Pointer

-- New type wrappers in order to avoid orphan instances
newtype WrapInit = WrapInit Parse.Init

newtype WrapUpdate = WrapUpdate Update.Update deriving newtype (IUpdate)

-----------------------------Instance------------------------------------------
instance IBot Pointer WrapInit WrapUpdate where
  getInit :: MT m => Pointer -> m WrapInit
  getInit _ = WrapInit <$> _getInit

  getUpdateId :: WrapInit -> UpdateId
  getUpdateId (WrapInit ini) = Parse.ts ini

  setUpdateId :: WrapInit -> UpdateId -> WrapInit
  setUpdateId (WrapInit ini) newts = WrapInit $ ini {Parse.ts = newts}

  getUpdates :: MT m => WrapInit -> m ([WrapUpdate], WrapInit)
  getUpdates (WrapInit ini) = do
    (us, newini) <- _getUpdates ini
    return (WrapUpdate <$> us, WrapInit newini)

  sendMessage :: MT m => WrapUpdate -> [Label] -> Int -> m ()
  sendMessage (WrapUpdate u) = _sendMessage u

--------------------------------Internal functions----------------------------------------
-- Initialization - get last updateId, server name, key for getUpdates request
_getInit :: MT m => m Parse.Init
_getInit = do
  Log.setSettings Color.Blue True "getInit"
  Cache.ConfigApp _name _host tk _updateId _ _repeatNumber gid v <- Cache.getConfigApp
  let api = API.API API.Groups API.GetLongPollServer
  Log.send
  json <- Request.api api (Query.getLongPollServer tk gid v) False
  Log.receive
  o <- Parse.getObject json
  Log.receiveData "object" o
  ini@(Parse.Init _server _key _ts) <- Parse.init o
  Log.receiveData "init" ini
  return ini

-- Get updates from messenger server by the long polling method
_getUpdates :: MT m => Parse.Init -> m ([Update.Update], Parse.Init)
_getUpdates ini@(Parse.Init server0 _ ts0) = do
  Log.setSettings Color.Cyan True "getUpdates"
  let query = Query.longPoll ini 25
  (host, path) <- parseServer server0
  let request = Request.build host path query
  Log.send
  json <- Request.send request True -- long polling
  Log.receive
  o <- Parse.getObject json
  Log.receiveData "object" o
  muid <- Parse.updateId o
  let newIni = ini {Parse.ts = fromMaybe ts0 muid}
  Log.receiveData "updateId" muid
  us <- Parse.updates o
  Log.receiveData "updates" us
  return (us, newIni)

-- Send response to a single user
_sendMessage :: MT m => Update.Update -> [Label] -> Int -> m ()
_sendMessage update btns rn = do
  Log.setSettings Color.Yellow True "sendMessage"
  Cache.ConfigApp _name _host tk _updateId _ _repeatNumber _groupId v <- Cache.getConfigApp
  Log.send
  printT btns
  query <- Query.sendMessage tk v update btns
  Log.debugM update
  forM_ [1 .. rn] $ \_ -> do
    Log.receiveData "query" query
    json <- Request.api (API.API API.Messages API.Send) query False
    Log.receive
    o <- Parse.getObject json
    Log.receiveData "object" o

-- "https://lp.vk.com/wh777777777" -> "lp.vk.com" "/wh777777777"
parseServer :: MError m => String -> m (Cache.Host, Path)
parseServer url = do
  let err =
        Error.QueryError $
          template "Could not match pattern \"https://{host}/{path}\" in long polling server \"{0}\"" [url]
  let strs = splitOn "//" url
  case strs of
    [https, str] -> do
      if https /= "https:"
        then Error.throw err
        else do
          let strs1 = splitOn "/" str
          case strs1 of
            [host, path] -> return (host, '/' : path)
            _ -> Error.throw err
    _ -> Error.throw err