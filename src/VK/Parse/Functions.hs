module VK.Parse.Functions where

import Common.Misc (UpdateId)
import Data.Aeson (Object)
import Interface.Class (MError)
import Logic.Parse.Internal (parseE)
import VK.Parse.Internal (parseInit, parseUpdateId, parseUpdates)
import VK.Update (Init, Update)

init :: MError m => Object -> m Init
init = parseE parseInit

updateId :: MError m => Object -> m (Maybe UpdateId)
updateId = parseE parseUpdateId

updates :: MError m => Object -> m [Update]
updates = parseE parseUpdates