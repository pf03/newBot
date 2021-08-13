module Common.Types where

import Common.Convert (Convert)
import Data.Aeson (FromJSON, FromJSONKey, ToJSON, ToJSONKey)
import Data.String (IsString)

newtype Path = Path String deriving newtype (Show, FromJSON, ToJSON)

newtype Message = Message String deriving newtype (Show, FromJSON, ToJSON, Convert, Eq, IsString)

newtype Label = Label String deriving newtype (Show, FromJSON, ToJSON, Eq)

data Command = Help | Repeat | Start | Unknown String | Button Int deriving (Show, Eq)

newtype UpdateId = UpdateId Int deriving newtype (Show, FromJSON, ToJSON, Convert, Num, Read, Ord, Eq)

newtype ChatId = ChatId Int deriving newtype (Show, FromJSON, ToJSON, ToJSONKey, FromJSONKey, Convert, Ord, Eq, Num)

newtype Key = Key String deriving newtype (Show, FromJSON, ToJSON, IsString)

newtype UserId = UserId Int deriving newtype (Show, FromJSON, ToJSON, Convert)

newtype WallId = WallId Int deriving newtype (Show, FromJSON, ToJSON, Convert)

newtype ItemId = ItemId Int deriving newtype (Show, FromJSON, ToJSON, Convert)

newtype EntityId = EntityId Int deriving newtype (Show, FromJSON, ToJSON, Convert)

newtype PollId = PollId String deriving newtype (Show, FromJSON, ToJSON, Convert)

newtype FileId = FileId String deriving newtype (Show, FromJSON, ToJSON, Convert)

newtype Url = Url String deriving newtype (Show, FromJSON, ToJSON)

newtype ItemName = ItemName String deriving newtype (Show, FromJSON, ToJSON, IsString)

-- time out for long polling
newtype TimeOut = TimeOut Int deriving newtype (Show, Num, Convert)

newtype Token = Token String deriving newtype (Show, FromJSON, ToJSON, IsString)

newtype Host = Host String deriving newtype (Show, FromJSON, ToJSON, IsString)