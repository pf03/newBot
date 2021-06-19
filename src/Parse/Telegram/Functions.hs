{-# LANGUAGE OverloadedStrings #-}

module Parse.Telegram.Functions where

import Class (MError)
import Common.Types (UpdateId)
import Data.Aeson (Object)
import qualified Messenger.Update.Telegram.Types as Update
import Parse.Internal (parseE)
import Parse.Telegram.Internal (parseUpdateIds, parseUpdates)

updateId :: MError m => Object -> m (Maybe UpdateId)
updateId object = do
  updateIds <- parseE parseUpdateIds object
  case updateIds of
    [] -> return Nothing
    _ -> return $ Just $ maximum updateIds

updates :: MError m => Object -> m [Update.Update]
updates = parseE parseUpdates