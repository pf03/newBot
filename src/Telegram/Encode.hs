module Telegram.Encode where

import Common.Misc (LBS)
import Data.Aeson (KeyValue ((.=)), Value (Array, String), encode, object)
import Data.Text (pack)
import GHC.Exts (IsList (fromList))

keyboard :: [String] -> LBS
keyboard strs = encode $ object ["keyboard" .= Array (fromList [Array $ fromList (arr strs)])]
  where
    arr :: [String] -> [Value]
    arr = map (\str -> object ["text" .= str])

pollOptions :: [String] -> LBS
pollOptions ops = encode $ Array $ fromList (map (String . pack) ops)