{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}

module Messenger.Bot.Class where

import Common.Types (Label, UpdateId)
import Messenger.Update.Class (IUpdate)
import Transformer.Types (Transformer)

class (IUpdate (UpdateType pointer)) => IBot pointer where
  type UpdateType pointer
  type InitType pointer
  getInit :: pointer -> Transformer (InitType pointer)
  getMUpdateId :: pointer -> InitType pointer -> Maybe UpdateId
  getUpdates :: pointer -> InitType pointer -> Transformer ([UpdateType pointer], InitType pointer)
  sendMessage :: pointer -> UpdateType pointer -> [Label] -> Transformer ()