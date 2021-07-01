module Logic.Telegram.Encode where

import Common.Types (Label)
import Data.Aeson (KeyValue ((.=)), Value (Array, String), encode, object)
import qualified Data.ByteString.Lazy.Char8 as LC
import Data.Text (pack)
import GHC.Exts (IsList (fromList))

encodeKeyboard :: [Label] -> LC.ByteString
encodeKeyboard strs = encode $ object ["keyboard" .= Array (fromList [Array $ fromList (arr strs)])]
  where
    arr :: [Label] -> [Value]
    arr = map (\str -> object ["text" .= str])

encodePollOptions :: [String] -> LC.ByteString
encodePollOptions options = encode $ Array $ fromList (map (String . pack) options)