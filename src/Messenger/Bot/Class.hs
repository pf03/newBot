module Messenger.Bot.Class where

import Common.Types (Label, UpdateId)
import Messenger.Update.Class (IUpdate)
import Transformer.Types ( BotStateIO )

class (IUpdate (UpdateType pointer)) => IBot pointer where
  type UpdateType pointer
  type InitType pointer
  getInit :: pointer -> BotStateIO (InitType pointer)
  getMUpdateId :: pointer -> InitType pointer -> Maybe UpdateId
  getUpdates :: pointer -> InitType pointer -> BotStateIO ([UpdateType pointer], InitType pointer)
  sendMessage :: pointer -> UpdateType pointer -> [Label] -> BotStateIO ()