{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Common.Types where

import Common.Convert ( Convert )
import GHC.Generics (Generic)
import Data.Aeson ( FromJSON, ToJSON, ToJSONKey, FromJSONKey )
import Data.String ( IsString )

newtype Path = Path String deriving newtype (Show, FromJSON, ToJSON)

newtype Message = Message String deriving newtype (Show, FromJSON, ToJSON, Convert)

newtype Label = Label String deriving newtype (Show, FromJSON, ToJSON)

data Command = Help | Repeat | Start | Unknown String | Button Int deriving (Show, Eq)

newtype UpdateId = UpdateId Int deriving newtype (Show, FromJSON, ToJSON, Convert, Num, Read, Ord, Eq)

newtype ChatId = ChatId Int deriving newtype (Show, FromJSON, ToJSON, ToJSONKey, FromJSONKey, Convert, Ord, Eq)

newtype Key = Key String deriving newtype (Show, FromJSON, ToJSON, IsString)

newtype UserId = UserId Int deriving newtype (Show, FromJSON, ToJSON, Convert)

newtype IntId = IntId Int deriving newtype (Show, FromJSON, ToJSON, Convert)

newtype StrId = StrId String deriving newtype (Show, FromJSON, ToJSON, Convert)

newtype FileId = FileId String deriving newtype (Show, FromJSON, ToJSON, Convert)

newtype Url = Url String deriving newtype (Show, FromJSON, ToJSON)

newtype ItemName = ItemName String deriving newtype (Show, FromJSON, ToJSON, IsString)

-- time out for long polling
newtype TimeOut = TimeOut Int deriving newtype (Show, Num, Convert)

newtype Token = Token String deriving newtype (Show, FromJSON, ToJSON)

newtype Host = Host String deriving newtype (Show, FromJSON, ToJSON)

-- type Changed = Bool