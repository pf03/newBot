{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Messenger.API.VK.Types where

import Common.Types ( Path(Path) )
import Data.Char (toLower)
import Class ( IAPI )
import qualified Messenger.API.Class as Class

-----------------------------Types---------------------------------------------
data API = API APIGroup APIName

data APIGroup = Groups | Messages deriving (Show)

data APIName = GetLongPollServer | Send deriving (Show)

type Version = String

-----------------------------Instance------------------------------------------
instance IAPI API where
  apiName (API apiGroup apiName) = (toLower g : gs) ++ "." ++ (toLower n : ns)
    where
      (g : gs) = show apiGroup
      (n : ns) = show apiName

  getPath _ api = Path $ "/method/" ++ Class.apiName api