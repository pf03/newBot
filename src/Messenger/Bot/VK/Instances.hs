module Messenger.Bot.VK.Instances where

import Class (MTrans)
import Common.Types (Label)
import Data.Maybe (fromMaybe)
import qualified Interface.Cache.Exports as Cache
import qualified Interface.Log.Exports as Log
import qualified Logic.Request as Request
import qualified Logic.VK.Query.Functions as Query
import qualified Messenger.API.VK.Types as API
import qualified Messenger.Update.VK.Types as Update
import qualified Parse.VK.Exports as Parse
import qualified System.Console.ANSI as Color
import Prelude hiding (init)

-- Initialization - get last updateId, server name, key for getUpdates request
getInit :: MTrans m => m Update.Init
getInit = do
  Log.setSettings Color.Blue True "getInit"
  let api = API.API API.Groups API.GetLongPollServer
  Log.writeSending
  query <- Query.getLongPollServerQuery
  json <- Request.sendApiRequest api query False
  Log.writeReceiving
  object <- Parse.getObject json
  Log.writeReceivingData "object" object
  init@(Update.Init server key _) <- Parse.parseInit object
  Log.writeReceivingData "init" init
  mUpdateIdFromFile <- Cache.getMUpdateId
  case mUpdateIdFromFile of
    Nothing -> return init
    Just updateIdFromFile -> return $ Update.Init server key updateIdFromFile

-- Get updates from messenger server by the long polling method
getUpdates :: MTrans m => Update.Init -> m ([Update.Update], Update.Init)
getUpdates init@(Update.Init server _ ts) = do
  Log.setSettings Color.Cyan True "getUpdates"
  let query = Query.longPollQuery init 25
  request <- Request.parseRequest server query
  Log.writeSending
  json <- Request.sendRequest request True -- long polling
  Log.writeReceiving
  object <- Parse.getObject json
  Log.writeReceivingData "object" object
  mUpdateId <- Parse.parseUpdateId object
  let newInit = init {Update.ts = fromMaybe ts mUpdateId}
  Log.writeReceivingData "mUpdateId" mUpdateId
  updates <- Parse.parseUpdates object
  Log.writeReceivingData "updates" updates
  return (updates, newInit)

-- Send response to a single user
sendMessage :: MTrans m => Update.Update -> [Label] -> m ()
sendMessage update btns = do
  Log.setSettings Color.Yellow True "sendMessage"
  Log.writeSending
  query <- Query.sendMessageQuery update btns
  Log.writeDebugM update
  Log.writeReceivingData "query" query
  json <- Request.sendApiRequest (API.API API.Messages API.Send) query False
  Log.writeReceiving
  object <- Parse.getObject json
  Log.writeReceivingData "object" object