{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE TypeFamilies          #-}

module Messenger.Bot.Class where

import Common.Types ( Label, UpdateId )
import Messenger.Update.Class (IUpdate)
import Transformer.Types ( Transformer )

class (IUpdate (UpdateType pointer)) => IBot pointer where
  
  type UpdateType pointer
  type InitType pointer 

  -- Initial bot request to messenger server that returns the initialization data
  getInit :: pointer -> Transformer (InitType pointer)

  -- Get UpdateId from initialization data
  getMUpdateId :: pointer -> InitType pointer -> Maybe UpdateId

  -- Get updates from messenger server using initialization data
  getUpdates :: pointer -> InitType pointer -> Transformer ([UpdateType pointer], InitType pointer)

  -- Send response to messenger server using received updates
  sendMessage :: pointer -> UpdateType pointer -> [Label] -> Transformer ()

  -- getApiName :: pointer -> ApiType pointer -> String

  -- -- Get path for sending API request
  -- getApiPath :: pointer -> Token -> ApiType pointer -> Path