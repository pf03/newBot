module Telegram.Encode where

import Common.Types (LBS)
import Data.Aeson (KeyValue ((.=)), Value (Array, String), encode, object)
import Data.Text (pack)
import GHC.Exts (IsList (fromList))

keyboard :: [String] -> LBS
keyboard strs = encode $ object ["keyboard" .= Array (fromList [Array $ fromList (arr strs)])]
  where
    arr :: [String] -> [Value]
    arr = map (\str -> object ["text" .= str])

pollOptions :: [String] -> LBS
pollOptions options = encode $ Array $ fromList (map (String . pack) options)