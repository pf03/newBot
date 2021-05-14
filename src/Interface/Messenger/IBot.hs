{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}

module Interface.Messenger.IBot where

import Common.Misc (Label, UpdateId)
import Interface.MT (MT)
import Interface.Messenger.IUpdate (IUpdate)

class (IUpdate update) => IBot pointer init update | pointer -> init, init -> update, update -> pointer where
  -- Initial bot request to messenger server that returns the initialization data
  getInit :: MT m => pointer -> m init

  -- Get UpdateId from initialization data
  getUpdateId :: init -> UpdateId

  -- Set UpdateId to initialization data
  setUpdateId :: init -> UpdateId -> init

  -- Get updates from messenger server using initialization data
  getUpdates :: MT m => init -> m ([update], init)

  -- Send response to messenger server using received updates
  sendMessage :: MT m => update -> [Label] -> m ()