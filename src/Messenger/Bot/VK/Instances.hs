{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE TypeFamilies          #-}

module Messenger.Bot.VK.Instances where

import Common.Types ( UpdateId, Label ) 
import Messenger.Bot.Class (IBot(..))
import qualified Messenger.Bot.VK.Internal as Internal
import qualified Messenger.Update.VK.Types as Update
import Messenger.Update.VK.Types ( Init, Update )
import Prelude hiding (init)
import Transformer.Types ( Transformer )

data Pointer = Pointer

instance IBot Pointer where

  type UpdateType Pointer = Update
  type InitType Pointer = Init

  getInit :: Pointer -> Transformer Init
  getInit _ = Internal.getInit

  getMUpdateId :: Pointer -> Init -> Maybe UpdateId
  getMUpdateId _ init = Just $ Update.ts init

  getUpdates :: Pointer -> Init -> Transformer ([Update], Init)
  getUpdates _ = Internal.getUpdates

  sendMessage :: Pointer -> Update -> [Label] -> Transformer ()
  sendMessage _ update = Internal.sendMessage update