{-# LANGUAGE MultiParamTypeClasses #-}
--{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
-- {-# LANGUAGE DeriveGeneric #-}
--importPriority = 59
module VK.API where

-- Our modules
import Interface.Messenger.IAPI as API  --60

-- Other modules
import Data.Char
-- import GHC.Generics
-- import Data.Aeson
-- import Common.Misc

-----------------------------Types---------------------------------------------
data API =  API APIGroup APIName
data APIGroup = Groups | Messages deriving Show
data APIName = GetLongPollServer | Send deriving Show


-------------------------instance App.API---------------------------------------
instance IAPI API where
    apiName (API apiGroup apiName) =  let 
        (g:gs) = show apiGroup;
        (n:ns) = show apiName in 
        (toLower g:gs)++ "." ++ (toLower n:ns)
    getPath token api = "/method/" ++ apiName api
