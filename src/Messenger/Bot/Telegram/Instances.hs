module Messenger.Bot.Telegram.Instances where

import Common.Types (Label, UpdateId)
import Messenger.Bot.Class (IBot (..))
import qualified Messenger.Bot.Telegram.Internal as Internal
import Messenger.Bot.Telegram.Types (Init (..))
import Messenger.Update.Telegram.Types (Update)
import Transformer.Types (BotStateIO)

data Pointer = Pointer

instance IBot Pointer where
  type UpdateType Pointer = Update
  type InitType Pointer = Init

  getInit :: Pointer -> BotStateIO Init
  getInit _ = Init <$> Internal.getUpdateId

  getMUpdateId :: Pointer -> Init -> Maybe UpdateId
  getMUpdateId _ (Init mUpdateId) = mUpdateId

  getUpdates :: Pointer -> Init -> BotStateIO ([Update], Init)
  getUpdates _ (Init mUpdateId) = do
    (updates, newMUpdateId) <- Internal.getUpdates mUpdateId
    return (updates, Init newMUpdateId)

  sendMessage :: Pointer -> Update -> [Label] -> BotStateIO ()
  sendMessage _ update btns = Internal.sendMessage update btns