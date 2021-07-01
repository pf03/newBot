module Parse.VK.Functions where

import Class (MError)
import Common.Types (UpdateId)
import Data.Aeson (Object)
import qualified Messenger.Update.VK.Types as Update
import Parse.Internal (parseE)
import qualified Parse.VK.Internal as Internal

parseInit :: MError m => Object -> m Update.Init
parseInit = parseE Internal.parseInit

parseUpdateId :: MError m => Object -> m (Maybe UpdateId)
parseUpdateId = parseE Internal.parseUpdateId

parseUpdates :: MError m => Object -> m [Update.Update]
parseUpdates = parseE Internal.parseUpdates