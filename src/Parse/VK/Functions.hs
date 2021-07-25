module Parse.VK.Functions where

import Common.Types (UpdateId)
import Data.Aeson (Object)
import qualified Messenger.Update.VK.Types as Update
import Parse.Internal (parseE)
import qualified Parse.VK.Internal as Internal

parseInit :: Monad m => Object -> m Update.Init
parseInit = parseE Internal.parseInit

parseUpdateId :: Monad m => Object -> m (Maybe UpdateId)
parseUpdateId = parseE Internal.parseUpdateId

parseUpdates :: Monad m => Object -> m [Update.Update]
parseUpdates = parseE Internal.parseUpdates