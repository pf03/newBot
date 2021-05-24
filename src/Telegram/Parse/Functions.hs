{-# LANGUAGE OverloadedStrings #-}

module Telegram.Parse.Functions where

import Common.Misc ( UpdateId )
import Data.Aeson ( Object )
import Interface.Class (MError)
import Logic.Parse.Internal (parseE)
import Telegram.Parse.Internal (parseUpdateIds, parseUpdates)
import qualified Telegram.Update as Update

updateId :: MError m => Object -> m (Maybe UpdateId)
updateId o = do
  ids <- parseE parseUpdateIds o
  case ids of
    [] -> return Nothing
    _ -> return $ Just $ maximum ids

updates :: MError m => Object -> m [Update.Update]
updates = parseE parseUpdates