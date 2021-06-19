module Logic.VK.Encode where

import Common.Types (IntId, Label, Url) --( IntId, Label )
import Data.Aeson (KeyValue ((.=)), Value (Array), encode, object)
import qualified Data.ByteString.Lazy.Char8 as LC
import GHC.Exts (IsList (fromList))
import Messenger.Update.VK.Types (GroupId, OwnerId)

keyboard :: [Label] -> LC.ByteString
keyboard strs =
  encode $
    object
      [ "one_time" .= True,
        "inline" .= False,
        "buttons" .= Array (fromList [Array $ fromList (_buttons strs)])
      ]  where
    _buttons :: [Label] -> [Value]
    _buttons = map $ \str ->
      object
        [ "action"
            .= object
              [ "type" .= ("text" :: String),
                "payload" .= ("{}" :: String),
                "label" .= str
              ]
        ]

contentUrl :: Url -> LC.ByteString
contentUrl str =
  encode $
    object
      [ "type" .= ("url" :: String),
        "url" .= str
      ]

contentMessage :: OwnerId -> GroupId -> IntId -> LC.ByteString
contentMessage ownerId peerId messageId =
  encode $
    object
      [ "type" .= ("message" :: String),
        "owner_id" .= ownerId,
        "peer_id" .= peerId,
        "conversation_message_id" .= messageId
      ]
