module VK.Bot.Internal where

import Common.Types ( Path, Label )
import Common.Functions ( template)
import Data.List.Split (splitOn)
import Data.Maybe (fromMaybe)
import Interface.Class ( MTrans, MError )
import qualified Interface.MCache.Exports as Cache
import qualified Interface.MError.Exports as Error
import qualified Interface.MLog.Exports as Log
import qualified Logic.Request as Request
import qualified System.Console.ANSI as Color (Color (..))
import qualified VK.API as API
import qualified VK.Parse.Exports as Parse
import qualified VK.Query.Functions as Query
import qualified VK.Update as Update

-- Initialization - get last updateId, server name, key for getUpdates request
getInit :: MTrans m => m Update.Init
getInit = do
  Log.setSettings Color.Blue True "getInit"
  Cache.ConfigApp _enable _name _app _host tk _updateId _ _repeatNumber gid v <- Cache.getConfigApp
  let api = API.API API.Groups API.GetLongPollServer
  Log.send
  json <- Request.api api (Query.getLongPollServer tk gid v) False
  Log.receive
  o <- Parse.getObject json
  Log.receiveData "object" o
  ini@(Update.Init _server _key _ts) <- Parse.init o
  Log.receiveData "init" ini
  return ini

-- Get updates from messenger server by the long polling method
getUpdates :: MTrans m => Update.Init -> m ([Update.Update], Update.Init)
getUpdates ini@(Update.Init server0 _ ts0) = do
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
  let newIni = ini {Update.ts = fromMaybe ts0 muid}
  Log.receiveData "updateId" muid
  us <- Parse.updates o
  Log.receiveData "updates" us
  return (us, newIni)

-- Send response to a single user
sendMessage :: MTrans m => Update.Update -> [Label] -> m ()
sendMessage update btns = do
  Log.setSettings Color.Yellow True "sendMessage"
  Cache.ConfigApp _enable _name _app _host tk _updateId _ _repeatNumber _groupId v <- Cache.getConfigApp
  Log.send
  query <- Query.sendMessage tk v update btns
  Log.debugM update
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