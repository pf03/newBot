module Common.Convert where

import Data.Aeson (Object, Value, encode)
import qualified Data.ByteString.Char8 as BC
import qualified Data.ByteString.Lazy.Char8 as LC
import Data.Text (pack)
import Data.Text.Encoding (encodeUtf8)
import Network.HTTP.Simple (Query)

class Convert a where
  convert :: a -> BC.ByteString

instance Convert String where
  convert = encodeUtf8 . pack -- encodeUtf8 for correct cyrillic encoding

instance Convert LC.ByteString where
  convert = BC.pack . LC.unpack

instance Convert Int where
  convert = BC.pack . show

instance Convert Float where
  convert = BC.pack . show

instance Convert Value where
  convert = convert . encode

instance Convert Object where
  convert = convert . encode

jConvert :: Convert a => a -> Maybe BC.ByteString
jConvert = Just . convert

(<:>) :: Convert a => String -> a -> Query
(<:>) key value = [(convert key, jConvert value)]

infixr 7 <:>

(<:?>) :: Convert a => String -> Maybe a -> Query
(<:?>) key mValue = case mValue of
  Nothing -> []
  Just value -> [(convert key, jConvert value)]

infixr 7 <:?>