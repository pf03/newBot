module Telegram.Bot.Internal  where

import Common.Misc (Label, UpdateId, template)
import Control.Applicative (Alternative ((<|>)))
import Control.Monad (forM_)
import Interface.Class ( MT )
import qualified Interface.MCache.Exports as Cache
import qualified Interface.MLog.Exports as Log
import qualified Logic.Request as Request
import qualified System.Console.ANSI as Color (Color (..))
import qualified Telegram.API as API
import qualified Telegram.Parse as Parse
import qualified Telegram.Query as Query
import qualified Telegram.Update as Update


-- Initialization - get last updateId for getUpdates request
_getUpdateId :: MT m => m UpdateId
_getUpdateId = do
  Log.setSettings Color.Blue True "_getUpdateId"
  uidFromFile <- Cache.getUpdateIdFromFile
  if uidFromFile
    then return 0 -- if we get updateId from a file, then initialization is not needed
    else do
      Log.send
      (_, muid) <- _getUpdates Nothing
      Log.receive
      maybe _getUpdateId (return . (-) 1) muid

-- Get updates from messenger server by the long polling method
-- _getUpdates Nothing - for initialization
-- _getUpdates (Just uid) - for get updates
_getUpdates :: MT m => Maybe UpdateId -> m ([Update.Update], Maybe UpdateId)
_getUpdates muid = do
  Log.setSettings Color.Cyan True $ template "_getUpdates, muid = {0}" [show muid]
  Log.send
  response <- Request.api API.GetUpdates (Query.getUpdates (fmap (+ 1) muid) 25) True
  Log.receive
  o <- Parse.getObject response
  Log.receiveData "object -- convert" o
  mnewuid <- Parse.updateId o
  Log.receiveData "mnewuid" mnewuid
  us <- Parse.updates o
  Log.receiveData "update" us
  return (us, mnewuid <|> muid)

-- Send response to a single user
_sendMessage :: MT m => Update.Update -> [Label] -> Int -> m ()
_sendMessage update btns rn = do
  Log.setSettings Color.Yellow True "sendMessage"
  Log.send
  (api, query) <- Query.sendMessage update btns
  forM_ [1 .. rn] $ \_ -> do
    Log.receiveData "(api, query)" (api, query)
    json <- Request.api api query False
    Log.receive
    o <- Parse.getObject json
    Log.receiveData "object" o

-- Dumping messages that we cannot parse, for debugging purposes
reset :: MT m => m ()
reset = do
  uid <- Cache.getUpdateId
  Log.setSettings Color.Cyan True $ template "reset, uid = {0}" [show uid]
  Log.send
  json <- Request.api API.GetUpdates (Query.getUpdates (Just uid) 0) True
  Log.receive
  o <- Parse.getObject json
  mnewuid <- Parse.updateId o
  Log.receiveData "mnewuid" mnewuid