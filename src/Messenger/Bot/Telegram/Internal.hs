module Messenger.Bot.Telegram.Internal where

import Class (MCache, MLog, MTrans)
import Common.Functions (template)
import Common.Types ( Path(..), Token(..), UpdateId, Label )
import Control.Applicative (Alternative ((<|>)))
import Control.Monad.IO.Class
import qualified Data.ByteString.Lazy.Char8 as LC
import Data.Char (toLower)
import qualified Interface.Cache.Exports as Cache
import qualified Interface.Log.Exports as Log
import qualified Logic.Request as Request
import qualified Logic.Telegram.Query as Query
import Messenger.Bot.Telegram.Types
import qualified Messenger.Update.Telegram.Types as Update
import qualified Network.HTTP.Simple as HTTP
import qualified Parse.Telegram.Exports as Parse
import qualified System.Console.ANSI as Color

-- Initialization - get last updateId for getUpdates request
getUpdateId :: MTrans m => m (Maybe UpdateId)
getUpdateId = do
  Log.setSettings Color.Blue True "getUpdateId"
  Cache.getMUpdateId

getUpdates :: MTrans m => Maybe UpdateId -> m ([Update.Update], Maybe UpdateId)
getUpdates mUpdateId = do
  Log.setSettings Color.Cyan True $ template "getUpdates, mUpdateId = {0}" [show mUpdateId]
  Log.writeSending
  let query = Query.getUpdatesQuery (fmap (+ 1) mUpdateId) 25
  response <- sendApiRequest GetUpdates query True
  Log.writeReceiving
  object <- Parse.getObject response
  Log.writeReceivingData "object" object
  newMUpdateId <- Parse.parseUpdateId object
  Log.writeReceivingData "newMUpdateId" newMUpdateId
  updates <- Parse.parseUpdates object
  Log.writeReceivingData "update" updates
  return (updates, newMUpdateId <|> mUpdateId)

-- Send response to a single user
sendMessage :: MTrans m => Update.Update -> [Label] -> m ()
sendMessage update btns = do
  Log.setSettings Color.Yellow True "sendMessage"
  Log.writeSending
  (api, query) <- Query.sendMessageQuery update btns
  Log.writeReceivingData "(api, query)" (api, query)
  json <- sendApiRequest api query False
  Log.writeReceiving
  object <- Parse.getObject json
  Log.writeReceivingData "object" object

-- Dumping messages that we cannot parse, for debugging purposes
reset :: MTrans m => m ()
reset = do
  mUpdateId <- Cache.getMUpdateId
  Log.setSettings Color.Cyan True $ template "reset, mUpdateId = {0}" [show mUpdateId]
  Log.writeSending
  json <- sendApiRequest GetUpdates (Query.getUpdatesQuery mUpdateId 0) True
  Log.writeReceiving
  object <- Parse.getObject json
  newMUpdateId <- Parse.parseUpdateId object
  Log.writeReceivingData "newMUpdateId" newMUpdateId

sendApiRequest :: (MCache m, MonadIO m, MLog m) => API -> HTTP.Query -> Bool -> m LC.ByteString
sendApiRequest api query save = do
  host <- Cache.getHost
  token <- Cache.getToken
  let path = getApiPath token api
  let request = Request.buildRequest host path query
  Request.sendRequest request save

getApiName :: API -> String
getApiName api =
  let (x : xs) = show api
   in toLower x : xs

getApiPath :: Token -> API -> Path
getApiPath (Token token) api = Path $ "/bot" ++ token ++ "/" ++ getApiName api