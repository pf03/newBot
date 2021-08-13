module Logic.VK.Encode where

import Common.Types (Label, Url)
import Data.Aeson (KeyValue ((.=)), Value (Array), encode, object)
import qualified Data.ByteString.Lazy.Char8 as LC
import GHC.Exts (IsList (fromList))

encodeKeyboard :: [Label] -> LC.ByteString
encodeKeyboard strs =
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

encodeContentUrl :: Url -> LC.ByteString
encodeContentUrl str =
  encode $
    object
      [ "type" .= ("url" :: String),
        "url" .= str
      ]
