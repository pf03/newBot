{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}

module Messenger.Bot.Class where

import Common.Types (Label, UpdateId)
import Interface.MTrans (MTrans)
import Messenger.Update.Class (IUpdate)

class (IUpdate update) => IBot pointer init update | pointer -> init, init -> update, update -> pointer where
  -- Initial bot request to messenger server that returns the initialization data
  getInit :: MTrans m => pointer -> m init

  -- Get UpdateId from initialization data
  getmUpdateId :: init -> Maybe UpdateId

  -- Get updates from messenger server using initialization data
  getUpdates :: MTrans m => init -> m ([update], init)

  -- Send response to messenger server using received updates
  sendMessage :: MTrans m => update -> [Label]  -> m ()