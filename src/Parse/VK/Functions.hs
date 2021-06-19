module Parse.VK.Functions where

import Class (MError)
import Common.Types (UpdateId)
import Data.Aeson (Object)
import qualified Messenger.Update.VK.Types as Update
import Parse.Internal (parseE)
import Parse.VK.Internal (parseInit, parseUpdateId, parseUpdates)

init :: MError m => Object -> m Update.Init
init = parseE parseInit

updateId :: MError m => Object -> m (Maybe UpdateId)
updateId = parseE parseUpdateId

updates :: MError m => Object -> m [Update.Update]
updates = parseE parseUpdates