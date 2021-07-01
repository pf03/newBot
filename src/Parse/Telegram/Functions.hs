{-# LANGUAGE OverloadedStrings #-}

module Parse.Telegram.Functions where

import Class (MError)
import Common.Types (UpdateId)
import Data.Aeson (Object)
import qualified Messenger.Update.Telegram.Types as Update
import Parse.Internal (parseE)
import qualified Parse.Telegram.Internal as Internal

parseUpdateId :: MError m => Object -> m (Maybe UpdateId)
parseUpdateId object = do
  updateIds <- parseE Internal.parseUpdateIds object
  case updateIds of
    [] -> return Nothing
    _ -> return $ Just $ maximum updateIds

parseUpdates :: MError m => Object -> m [Update.Update]
parseUpdates = parseE Internal.parseUpdates