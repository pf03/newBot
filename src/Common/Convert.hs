{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}

module Common.Convert where

-- import Common.Types (BS, LBS)
import Data.Aeson (Object, Value, encode)
import qualified Data.ByteString.Char8 as BC
import qualified Data.ByteString.Lazy.Char8 as LC
import Data.Text (pack)
import Data.Text.Encoding (encodeUtf8)
import Network.HTTP.Simple (Query)

type BS = BC.ByteString

type LBS = LC.ByteString

class Convert a where
  convert :: a -> BS

instance Convert String where
  convert = encodeUtf8 . pack -- encodeUtf8 for correct cyrillic encoding

instance Convert LBS where
  convert = BC.pack . LC.unpack

instance Convert Int where
  convert = BC.pack . show

instance Convert Float where
  convert = BC.pack . show

instance Convert Value where
  convert = convert . encode

instance Convert Object where
  convert = convert . encode

jconvert :: Convert a => a -> Maybe BC.ByteString
jconvert = Just . convert

(<:>) :: Convert a => String -> a -> Query
(<:>) key value = [(convert key, jconvert value)]

infixr 7 <:>

(<:?>) :: Convert a => String -> Maybe a -> Query
(<:?>) key mvalue = case mvalue of
  Nothing -> []
  Just value -> [(convert key, jconvert value)]

infixr 7 <:?>