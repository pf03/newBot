{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Telegram.API where

import Data.Char (toLower)
import Interface.Class (IAPI)
import qualified Interface.Messenger.IAPI as IAPI

-----------------------------Types---------------------------------------------
data API
  = GetUpdates
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
  | ForwardMessage
  deriving (Show)

-----------------------------Instance----------------------------------------------
instance IAPI API where
  apiName api =
    let (x : xs) = show api
     in toLower x : xs
  getPath token api = "/bot" ++ token ++ "/" ++ IAPI.apiName api
