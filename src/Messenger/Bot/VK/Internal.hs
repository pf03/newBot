module Messenger.Bot.VK.Internal where

import Common.Types (Label, Path (..))
import Control.Exception (throwIO)
import Control.Monad.IO.Class (MonadIO (liftIO))
import qualified Data.ByteString.Lazy.Char8 as LC
import Data.Char (toLower)
import Data.Maybe (fromMaybe)
import qualified Interface.Cache.Exports as Cache
import qualified Interface.Log.Exports as Log
import qualified Logic.Request as Request
import qualified Logic.VK.Query.Functions as Query
import Messenger.Bot.VK.Types (API (..))
import qualified Messenger.Bot.VK.Types as Bot
import qualified Messenger.Update.VK.Types as Update
import qualified Network.HTTP.Simple as HTTP
import qualified Parse.VK.Exports as Parse
import Transformer.Types (BotStateIO)
import Prelude hiding (init)

-- Initialization - get last updateId, server name, key for getUpdates request
getInit :: BotStateIO Update.Init
getInit = 
  Log.withCustomSettings Log.BlueScheme True "getInit" $ do
    let api = Bot.API Bot.Groups Bot.GetLongPollServer
    Log.writeSending
    query <- Query.getLongPollServerQuery
    json <- sendApiRequest api query False
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
getUpdates :: Update.Init -> BotStateIO ([Update.Update], Update.Init)
getUpdates init@(Update.Init server _ ts) = 
  Log.withCustomSettings Log.CyanScheme True "getUpdates" $ do
    let query = Query.longPollQuery init 25
    request <- either (liftIO . throwIO) return (Request.parseRequest server query)
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
sendMessage :: Update.Update -> [Label] -> BotStateIO ()
sendMessage update btns =
  Log.withCustomSettings Log.YellowScheme True "sendMessage" $ do
    Log.writeSending
    eQuery <- Query.sendMessageQuery update btns
    query <- either (liftIO . throwIO) return eQuery
    Log.writeDebugM update
    Log.writeReceivingData "query" eQuery
    json <- sendApiRequest (Bot.API Bot.Messages Bot.Send) query False
    Log.writeReceiving
    object <- Parse.getObject json
    Log.writeReceivingData "object" object

sendApiRequest :: API -> HTTP.Query -> Bool -> BotStateIO LC.ByteString
sendApiRequest api query save = do
  host <- Cache.getHost
  let path = getApiPath api
  let request = Request.buildRequest host path query
  Request.sendRequest request save

getApiName :: API -> String
getApiName (API apiGroup apiName) = (toLower g : gs) ++ "." ++ (toLower n : ns)
  where
    (g : gs) = show apiGroup
    (n : ns) = show apiName

getApiPath :: API -> Path
getApiPath api = Path $ "/method/" ++ getApiName api