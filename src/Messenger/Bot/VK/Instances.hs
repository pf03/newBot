module Messenger.Bot.VK.Instances where

import Common.Types ( Host(..), Label, Path(..) )
import Common.Functions ( template)
import Data.List.Split (splitOn)
import Data.Maybe (fromMaybe)
import Class ( MTrans, MError )
import qualified Interface.Cache.Exports as Cache
import qualified Interface.Error.Exports as Error
import qualified Interface.Log.Exports as Log
import qualified Logic.Request as Request
import qualified System.Console.ANSI as Color
import qualified Messenger.API.VK.Types as API
import qualified Parse.VK.Exports as Parse
import qualified Logic.VK.Query.Functions as Query
import qualified Messenger.Update.VK.Types as Update
import Prelude hiding (init)

-- Initialization - get last updateId, server name, key for getUpdates request
getInit :: MTrans m => m Update.Init
getInit = do
  Log.setSettings Color.Blue True "getInit"
  let api = API.API API.Groups API.GetLongPollServer
  Log.send
  query <- Query.getLongPollServerQuery 
  json <- Request.sendApiRequest api query False
  Log.receive
  object <- Parse.getObject json
  Log.receiveData "object" object
  init@(Update.Init server key _) <- Parse.init object
  Log.receiveData "init" init
  mupdateIdFromFile <- Cache.getmUpdateId
  case mupdateIdFromFile of
    Nothing -> return init
    Just updateIdFromFile -> return $ Update.Init server key updateIdFromFile

-- Get updates from messenger server by the long polling method
getUpdates :: MTrans m => Update.Init -> m ([Update.Update], Update.Init)
getUpdates init@(Update.Init server _ ts) = do
  Log.setSettings Color.Cyan True "getUpdates"
  let query = Query.longPollQuery init 25
  (host, path) <- parseServer server
  let request = Request.buildRequest host path query
  Log.send
  json <- Request.sendRequest request True -- long polling
  Log.receive
  object <- Parse.getObject json
  Log.receiveData "object" object
  mUpdateId <- Parse.updateId object
  let newInit = init {Update.ts = fromMaybe ts mUpdateId}
  Log.receiveData "mUpdateId" mUpdateId
  updates <- Parse.updates object
  Log.receiveData "updates" updates
  return (updates, newInit)

-- Send response to a single user
sendMessage :: MTrans m => Update.Update -> [Label] -> m ()
sendMessage update btns = do
  Log.setSettings Color.Yellow True "sendMessage"
  Log.send
  query <- Query.sendMessageQuery update btns
  Log.debugM update
  Log.receiveData "query" query
  json <- Request.sendApiRequest (API.API API.Messages API.Send) query False
  Log.receive
  object <- Parse.getObject json
  Log.receiveData "object" object

-- "https://lp.vk.com/wh777777777" -> "lp.vk.com" "/wh777777777"
parseServer :: MError m => String -> m (Host, Path)
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
            [host, path] -> return (Host host, Path $ '/' : path)
            _ -> Error.throw err
    _ -> Error.throw err