{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}

module Common.Types where

import qualified Data.ByteString.Char8 as BC
import qualified Data.ByteString.Lazy.Char8 as LC

type BS = BC.ByteString

type LBS = LC.ByteString

type Path = String

type Message = String

type Label = String

data Command = Help | Repeat | Start | Unknown String | Button Int deriving (Show, Eq)

type UpdateId = Int

type ChatId = Int

type Key = String

type UserId = Int

type IntId = Int

type StrId = String

type FileId = String

type Url = String

type ItemName = String

type TimeOut = Int -- time out long polling