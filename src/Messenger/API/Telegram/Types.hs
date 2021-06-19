{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Messenger.API.Telegram.Types where

import Common.Types ( Token(Token), Path(Path) )
import Data.Char (toLower)
import Messenger.API.Class ( IAPI )
import qualified Messenger.API.Class as Class

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
  getPath (Token token) api = Path $ "/bot" ++ token ++ "/" ++ Class.apiName api
