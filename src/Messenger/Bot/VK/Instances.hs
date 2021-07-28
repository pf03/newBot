{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies          #-}

module Messenger.Bot.VK.Instances where

import Class (IBot, MTrans)
import Common.Types ( UpdateId, Label ) 
import qualified Messenger.Bot.Class as Class
import qualified Messenger.Bot.VK.Internal as Internal
import qualified Messenger.Update.VK.Types as Update
import Messenger.Bot.VK.Types
import Messenger.Update.VK.Types
import Prelude hiding (init)

data Pointer = Pointer

instance IBot Pointer where

  type UpdateType Pointer = Update
  type InitType Pointer = Init
  type ApiType Pointer = API

  getInit :: MTrans m => Pointer -> m Init
  getInit _ = Internal.getInit

  getMUpdateId :: Pointer -> Init -> Maybe UpdateId
  getMUpdateId _ init = Just $ Update.ts init

  getUpdates :: MTrans m => Pointer -> Init -> m ([Update], Init)
  getUpdates _ = Internal.getUpdates

  sendMessage :: MTrans m => Pointer -> Update -> [Label] -> m ()
  sendMessage _ update = Internal.sendMessage update

  -- getApiName :: Pointer -> API -> String
  -- getApiName _ (API apiGroup apiName) = (toLower g : gs) ++ "." ++ (toLower n : ns)
  --   where
  --     (g : gs) = show apiGroup
  --     (n : ns) = show apiName

  -- getApiPath :: Pointer -> Token -> API -> Path
  -- getApiPath pointer _ api = Path $ "/method/" ++ Class.getApiName pointer api