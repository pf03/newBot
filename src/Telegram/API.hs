{-# LANGUAGE MultiParamTypeClasses #-}
--{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}

--importPriority = 59
module Telegram.API where

-- Our modules
import Interface.Messenger.IAPI as API

-- Other modules
import Data.Char

-----------------------------Types-----------------------------------
--можно сделать чтото типа 
--data API = GetUpdates | Send Entity
data API =  GetUpdates | SendMessage | SendSticker | SendAnimation | SendPhoto | SendVideo |SendDocument| SendPoll | SendContact | SendLocation
    | CopyMessage | ForwardMessage  deriving Show

-------------------------instance App.API---------------------------------------
instance IAPI API where
    apiName api = let 
        (x:xs) = show api 
        in (toLower x):xs
    getPath token api = "/bot"++ token ++"/" ++ apiName api