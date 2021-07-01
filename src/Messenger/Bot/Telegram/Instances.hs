module Messenger.Bot.Telegram.Instances where

import Class (MTrans)
import Common.Functions (template)
import Common.Types (Label, UpdateId)
import Control.Applicative (Alternative ((<|>)))
import qualified Interface.Cache.Exports as Cache
import qualified Interface.Log.Exports as Log
import qualified Logic.Request as Request
import qualified Logic.Telegram.Query as Query
import qualified Messenger.API.Telegram.Types as API
import qualified Messenger.Update.Telegram.Types as Update
import qualified Parse.Telegram.Exports as Parse
import qualified System.Console.ANSI as Color

-- Initialization - get last updateId for getUpdates request
getUpdateId :: MTrans m => m (Maybe UpdateId)
getUpdateId = do
  Log.setSettings Color.Blue True "getUpdateId"
  Cache.getmUpdateId

getUpdates :: MTrans m => Maybe UpdateId -> m ([Update.Update], Maybe UpdateId)
getUpdates mUpdateId = do
  Log.setSettings Color.Cyan True $ template "getUpdates, mUpdateId = {0}" [show mUpdateId]
  Log.writeSending
  let query = Query.getUpdatesQuery (fmap (+ 1) mUpdateId) 25
  response <- Request.sendApiRequest API.GetUpdates query True
  Log.writeReceiving
  object <- Parse.getObject response
  Log.writeReceivingData "object" object
  newmUpdateId <- Parse.updateId object
  Log.writeReceivingData "newmUpdateId" newmUpdateId
  updates <- Parse.updates object
  Log.writeReceivingData "update" updates
  return (updates, newmUpdateId <|> mUpdateId)

-- Send response to a single user
sendMessage :: MTrans m => Update.Update -> [Label] -> m ()
sendMessage update btns = do
  Log.setSettings Color.Yellow True "sendMessage"
  Log.writeSending
  (api, query) <- Query.sendMessageQuery update btns
  Log.writeReceivingData "(api, query)" (api, query)
  json <- Request.sendApiRequest api query False
  Log.writeReceiving
  object <- Parse.getObject json
  Log.writeReceivingData "object" object

-- Dumping messages that we cannot parse, for debugging purposes
reset :: MTrans m => m ()
reset = do
  mUpdateId <- Cache.getmUpdateId
  Log.setSettings Color.Cyan True $ template "reset, mUpdateId = {0}" [show mUpdateId]
  Log.writeSending
  json <- Request.sendApiRequest API.GetUpdates (Query.getUpdatesQuery mUpdateId 0) True
  Log.writeReceiving
  object <- Parse.getObject json
  newmUpdateId <- Parse.updateId object
  Log.writeReceivingData "mnewuid" newmUpdateId