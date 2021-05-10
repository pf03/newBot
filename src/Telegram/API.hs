{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Telegram.API where

-- Our modules
import           Interface.Messenger.IAPI as API

-- Other modules
import           Data.Char                (toLower)

-----------------------------Types-----------------------------------
data API =  GetUpdates
    | SendMessage
    | SendSticker
    | SendAnimation
    | SendPhoto
    | SendVideo
    | SendDocument
    | SendPoll
    | SendContact
    | SendLocation
    | CopyMessage
    | ForwardMessage  deriving Show

-----------------------------instance----------------------------------------------
instance IAPI API where
    apiName api = let
        (x:xs) = show api
        in toLower x:xs
    getPath token api = "/bot"++ token ++"/" ++ apiName api
