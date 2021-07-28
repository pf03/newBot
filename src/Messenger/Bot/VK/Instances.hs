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
import Prelude hiding (init)

data Pointer = Pointer

instance IBot Pointer where

  type UpdateType Pointer = WrapUpdate
  type InitType Pointer = WrapInit
  type ApiType Pointer = API

  getInit :: MTrans m => Pointer -> m WrapInit
  getInit _ = WrapInit <$> Internal.getInit

  getMUpdateId :: Pointer -> WrapInit -> Maybe UpdateId
  getMUpdateId _ (WrapInit init) = Just $ Update.ts init

  getUpdates :: MTrans m => Pointer -> WrapInit -> m ([WrapUpdate], WrapInit)
  getUpdates _ (WrapInit init) = do
    (updates, newInit) <- Internal.getUpdates init
    return (WrapUpdate <$> updates, WrapInit newInit)

  sendMessage :: MTrans m => Pointer -> WrapUpdate -> [Label] -> m ()
  sendMessage _ (WrapUpdate update) = Internal.sendMessage update

  -- getApiName :: Pointer -> API -> String
  -- getApiName _ (API apiGroup apiName) = (toLower g : gs) ++ "." ++ (toLower n : ns)
  --   where
  --     (g : gs) = show apiGroup
  --     (n : ns) = show apiName

  -- getApiPath :: Pointer -> Token -> API -> Path
  -- getApiPath pointer _ api = Path $ "/method/" ++ Class.getApiName pointer api