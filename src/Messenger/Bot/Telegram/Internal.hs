module Messenger.Bot.Telegram.Internal where

import Common.Functions (template)
import Common.Types (Label, Path (..), Token (..), UpdateId)
import Control.Applicative (Alternative ((<|>)))
import Control.Exception (throwIO)
import Control.Monad.IO.Class (MonadIO (liftIO))
import qualified Data.ByteString.Lazy.Char8 as LC
import Data.Char (toLower)
import qualified Interface.Cache.Exports as Cache
import qualified Interface.Log.Exports as Log
import qualified Logic.Request as Request
import qualified Logic.Telegram.Query as Query
import Messenger.Bot.Telegram.Types (API (GetUpdates))
import qualified Messenger.Update.Telegram.Types as Update
import qualified Network.HTTP.Simple as HTTP
import qualified Parse.Telegram.Exports as Parse
import Transformer.Types (BotStateIO)

-- Initialization - get last updateId for getUpdates request
getUpdateId :: BotStateIO (Maybe UpdateId)
getUpdateId = Cache.getMUpdateId

getUpdates :: Maybe UpdateId -> BotStateIO ([Update.Update], Maybe UpdateId)
getUpdates mUpdateId =
  Log.withCustomSettings Log.CyanScheme True (template "getUpdates, mUpdateId = {0}" [show mUpdateId]) $ do
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

sendMessage :: Update.Update -> [Label] -> BotStateIO ()
sendMessage update btns = do
  Log.withCustomSettings Log.YellowScheme True "sendMessage" $ do
    Log.writeSending
    (api, query) <- either (liftIO . throwIO) return (Query.sendMessageQuery update btns)
    Log.writeReceivingData "(api, query)" (api, query)
    json <- sendApiRequest api query False
    Log.writeReceiving
    object <- Parse.getObject json
    Log.writeReceivingData "object" object

-- Dumping messages that we cannot parse, for debugging purposes
reset :: BotStateIO ()
reset = do
  mUpdateId <- Cache.getMUpdateId
  Log.withCustomSettings Log.CyanScheme True (template "reset, mUpdateId = {0}" [show mUpdateId]) $ do
    Log.writeSending
    json <- sendApiRequest GetUpdates (Query.getUpdatesQuery mUpdateId 0) True
    Log.writeReceiving
    object <- Parse.getObject json
    newMUpdateId <- Parse.parseUpdateId object
    Log.writeReceivingData "newMUpdateId" newMUpdateId

sendApiRequest :: API -> HTTP.Query -> Bool -> BotStateIO LC.ByteString
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