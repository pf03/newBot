{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
module Interface.Messenger.IBot where

-- Our modules
import Interface.MT
import Interface.Messenger.IUpdate
import Common.Misc

class (IUpdate update) => IBot pointer init update  | pointer -> init , init -> update, update -> pointer where 
    getInit :: MT m => pointer -> m init
    getUpdateId :: init -> UpdateId
    setUpdateId :: init -> UpdateId -> init
    getUpdates:: MT m => init -> m ([update], init)
    sendMessage:: MT m => update -> [Label] -> m ()