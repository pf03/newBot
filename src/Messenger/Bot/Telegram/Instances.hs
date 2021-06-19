module Messenger.Bot.Telegram.Instances  where

import Common.Types ( UpdateId, Label )
import Common.Functions (template)
import Control.Applicative (Alternative ((<|>)))
import Class ( MTrans )
import qualified Interface.Cache.Exports as Cache
import qualified Interface.Log.Exports as Log
import qualified Logic.Request as Request
import qualified System.Console.ANSI as Color
import qualified Messenger.API.Telegram.Types as API
import qualified Parse.Telegram.Exports as Parse
import qualified Logic.Telegram.Query as Query
import qualified Messenger.Update.Telegram.Types as Update


-- Initialization - get last updateId for getUpdates request
getUpdateId :: MTrans m => m (Maybe UpdateId)
getUpdateId = do
  Log.setSettings Color.Blue True "getUpdateId"
  Cache.getmUpdateId

getUpdates :: MTrans m => Maybe UpdateId -> m ([Update.Update], Maybe UpdateId)
getUpdates mUpdateId = do
  Log.setSettings Color.Cyan True $ template "getUpdates, mUpdateId = {0}" [show mUpdateId]
  Log.send
  let query = Query.getUpdatesQuery (fmap (+ 1) mUpdateId) 25
  response <- Request.sendApiRequest API.GetUpdates query True
  Log.receive
  object <- Parse.getObject response
  Log.receiveData "object" object
  newmUpdateId <- Parse.updateId object
  Log.receiveData "newmUpdateId" newmUpdateId
  updates <- Parse.updates object
  Log.receiveData "update" updates
  return (updates, newmUpdateId <|> mUpdateId)

-- Send response to a single user
sendMessage :: MTrans m => Update.Update -> [Label] -> m ()
sendMessage update btns = do
  Log.setSettings Color.Yellow True "sendMessage"
  Log.send
  (api, query) <- Query.sendMessageQuery update btns
  Log.receiveData "(api, query)" (api, query)
  json <- Request.sendApiRequest api query False
  Log.receive
  object <- Parse.getObject json
  Log.receiveData "object" object

-- Dumping messages that we cannot parse, for debugging purposes
reset :: MTrans m => m ()
reset = do
  mUpdateId <- Cache.getmUpdateId
  Log.setSettings Color.Cyan True $ template "reset, mUpdateId = {0}" [show mUpdateId]
  Log.send
  json <- Request.sendApiRequest API.GetUpdates (Query.getUpdatesQuery mUpdateId 0) True
  Log.receive
  object <- Parse.getObject json
  newmUpdateId <- Parse.updateId object
  Log.receiveData "mnewuid" newmUpdateId