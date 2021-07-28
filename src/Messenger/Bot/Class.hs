-- {-# LANGUAGE AllowAmbiguousTypes   #-}
{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE FlexibleContexts      #-}
-- {-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies          #-}

module Messenger.Bot.Class where

import Common.Types ( Label, UpdateId )
import Interface.MTrans (MTrans)
import Messenger.Update.Class (IUpdate)

-- a = pointer
class (IUpdate (UpdateType pointer)) => IBot pointer where
  
  type UpdateType pointer
  type InitType pointer 

  -- Initial bot request to messenger server that returns the initialization data
  getInit :: MTrans m => pointer -> m (InitType pointer)

  -- Get UpdateId from initialization data
  getMUpdateId :: pointer -> InitType pointer -> Maybe UpdateId

  -- Get updates from messenger server using initialization data
  getUpdates :: MTrans m => pointer -> InitType pointer -> m ([UpdateType pointer], InitType pointer)

  -- Send response to messenger server using received updates
  sendMessage :: MTrans m => pointer -> UpdateType pointer -> [Label] -> m ()

  -- getApiName :: pointer -> ApiType pointer -> String

  -- -- Get path for sending API request
  -- getApiPath :: pointer -> Token -> ApiType pointer -> Path