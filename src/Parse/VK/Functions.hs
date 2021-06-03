module Parse.VK.Functions where

import Common.Types (UpdateId)
import Data.Aeson (Object)
import Interface.Class (MError)
import Parse.Internal (parseE)
import Parse.VK.Internal (parseInit, parseUpdateId, parseUpdates)
import qualified Messenger.Update.VK.Types as Update

init :: MError m => Object -> m Update.Init
init = parseE parseInit

updateId :: MError m => Object -> m (Maybe UpdateId)
updateId = parseE parseUpdateId

updates :: MError m => Object -> m [Update.Update]
updates = parseE parseUpdates