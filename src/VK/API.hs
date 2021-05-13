{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module VK.API where

-- Our modules
import           Interface.Messenger.IAPI as API

-- Other modules
import           Data.Char                (toLower)

-----------------------------Types---------------------------------------------
data API = API APIGroup APIName
data APIGroup = Groups | Messages deriving Show
data APIName = GetLongPollServer | Send deriving Show

-----------------------------Instance------------------------------------------
instance IAPI API where
    apiName (API apiGroup an) = (toLower g:gs) ++ "." ++ (toLower n:ns) where
        (g:gs) = show apiGroup;
        (n:ns) = show an

    getPath _ api = "/method/" ++ apiName api