module VK.Parse.Functions where

import Common.Misc (UpdateId)
import Data.Aeson (Object)
import Interface.Class (MError)
import Logic.Parse.Internal (_parseE)
import VK.Parse.Internal (_parseInit, _parseUpdateId, _parseUpdates)
import VK.Update (Init, Update)

init :: MError m => Object -> m Init
init = _parseE _parseInit

updateId :: MError m => Object -> m (Maybe UpdateId)
updateId = _parseE _parseUpdateId

updates :: MError m => Object -> m [Update]
updates = _parseE _parseUpdates