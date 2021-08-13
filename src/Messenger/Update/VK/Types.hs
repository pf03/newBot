module Messenger.Update.VK.Types where

import Common.Convert (Convert)
import Common.Types
  ( ChatId,
    Command,
    ItemId,
    ItemName,
    Key,
    Message,
    MessageOrCommand (..),
    UpdateId,
    Url,
    WallId,
  )
import Data.Aeson (FromJSON, ToJSON)
import GHC.Generics (Generic)
import Messenger.Update.Class (IUpdate)
import qualified Messenger.Update.Class as Class

-----------------------------Types---------------------------------------------
type Update = (ChatId, Entity)

data Entity = Entity {body :: MessageOrCommand, attachments :: [Attachment]} deriving (Show)

data Attachment
  = Sticker StickerId
  | Audio OwnerId OwnerId
  | Wall OwnerId WallId -- parser differs from Audio
  | Item ItemName OwnerId ItemId Key -- photo, video, doc
  | Link Url
  deriving (Show)

newtype StickerId = StickerId Int deriving newtype (Show, FromJSON, ToJSON, Convert)

newtype OwnerId = OwnerId Int deriving newtype (Show, FromJSON, ToJSON, Convert)

newtype GroupId = GroupId Int deriving newtype (Show, FromJSON, ToJSON, Convert)

data Init = Init {server :: String, key :: String, ts :: UpdateId} deriving (Show, Generic)

instance FromJSON Init

instance ToJSON Init

--------------------------instance App.Update------------------------------------
instance IUpdate Update where
  setMessage :: Update -> Message -> Update
  setMessage (chatId, Entity _ attachments0) message = (chatId, Entity (MessageEntity message) attachments0)

  getCommand :: Update -> Maybe Command
  getCommand (_, Entity (CommandEntity command) _) = Just command
  getCommand _ = Nothing

  getChatId :: Update -> ChatId
  getChatId = fst
