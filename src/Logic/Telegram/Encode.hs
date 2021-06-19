module Logic.Telegram.Encode where

import Common.Types (Label)
import Data.Aeson (KeyValue ((.=)), Value (Array, String), encode, object)
import qualified Data.ByteString.Lazy.Char8 as LC
import Data.Text (pack)
import GHC.Exts (IsList (fromList))

keyboard :: [Label] -> LC.ByteString
keyboard strs = encode $ object ["keyboard" .= Array (fromList [Array $ fromList (arr strs)])]
  where
    arr :: [Label] -> [Value]
    arr = map (\str -> object ["text" .= str])

pollOptions :: [String] -> LC.ByteString
pollOptions options = encode $ Array $ fromList (map (String . pack) options)