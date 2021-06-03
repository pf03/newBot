module VK.Encode
where

import Common.Types ( IntId, LBS )
import Data.Aeson ( encode, object, Value(Array), KeyValue((.=)) )
import GHC.Exts (IsList (fromList))
import Messenger.Update.VK.Types ( GroupId, OwnerId ) 

keyboard :: [String] -> LBS
keyboard strs =
  encode $
    object
      [ "one_time" .= True,
        "inline" .= False,
        "buttons" .= Array (fromList [Array $ fromList (_buttons strs)])
      ] 
  where
  _buttons :: [String] -> [Value]
  _buttons = map $ \str ->
    object
      [ "action"
          .= object
            [ "type" .= ("text" :: String),
              "payload" .= ("{}" :: String),
              "label" .= str
            ]
      ]
contentUrl :: String -> LBS
contentUrl str =
  encode $
    object
      [ "type" .= ("url" :: String),
        "url" .= str
      ]

contentMessage :: OwnerId -> GroupId -> IntId -> LBS
contentMessage ownerId peerId messageId =
  encode $
    object
      [ "type" .= ("message" :: String),
        "owner_id" .= ownerId,
        "peer_id" .= peerId,
        "conversation_message_id" .= messageId
      ]