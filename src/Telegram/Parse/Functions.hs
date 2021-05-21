{-# LANGUAGE OverloadedStrings #-}

module Telegram.Parse.Functions where

import Common.Misc ( UpdateId )
import Data.Aeson ( Object )
import Interface.Class (MError)
import Logic.Parse.Internal (_parseE)
import Telegram.Parse.Internal (_parseUpdateIds, _parseUpdates)
import Telegram.Update (Update)

updateId :: MError m => Object -> m (Maybe UpdateId)
updateId o = do
  ids <- _parseE _parseUpdateIds o
  case ids of
    [] -> return Nothing
    _ -> return $ Just $ maximum ids

updates :: MError m => Object -> m [Update]
updates = _parseE _parseUpdates