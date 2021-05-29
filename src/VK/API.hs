{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module VK.API where

import Data.Char (toLower)
import Interface.Class ( IAPI )
import qualified Interface.Messenger.IAPI as IAPI (IAPI (..))

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

  getPath _ api = "/method/" ++ IAPI.apiName api