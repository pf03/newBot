{-# LANGUAGE FlexibleInstances #-}

module Logic.Parse where

import Common.Misc (Convert (..), Key, LBS, jc)
import Data.Aeson (FromJSON (parseJSON), Object, Value (Object), eitherDecode, (.:), (.:?))
import Data.Aeson.Types (Parser, parseEither)
import Data.Maybe (fromJust, isJust)
import Data.Text (pack)
import Interface.Class (MError)
import qualified Interface.MError.Exports as Error
import Network.HTTP.Simple (Query)

-----------------------------From JSON-----------------------------------------
getObject :: MError m => LBS -> m Object
getObject bs = Error.catchEither (eitherDecode bs) Error.ParseError

getValue :: MError m => LBS -> m Value
getValue bs = Error.catchEither (eitherDecode bs) Error.ParseError

eDecode :: (MError m, FromJSON a) => LBS -> m a
eDecode bs = Error.catchEither (eitherDecode bs) Error.ParseError

-- Quitting the Parser monad
_parseE :: MError m => (Object -> Parser a) -> Object -> m a
_parseE f o = Error.catchEither (parseEither f o) Error.ParseError

_parseJSONo :: FromJSON a => Object -> Parser a
_parseJSONo = parseJSON . Object

-- | Wrapper for working with optional field
_mwithItem :: Key -> (Object -> Parser a) -> Object -> Parser (Maybe a)
_mwithItem k f o = do
  mo <- o .:? pack k
  case mo of
    Nothing -> return Nothing
    Just o1 -> Just <$> f o1

-- | Wrapper for working with internal lists
_withArrayItem :: Key -> (Object -> Parser a) -> Object -> Parser [a]
_withArrayItem k f o = do
  arr <- o .: pack k
  mapM f arr

-- | Wrapper for working with internal optional lists
_mwithArrayItem :: Key -> (Object -> Parser a) -> Object -> Parser (Maybe [a])
_mwithArrayItem k f o = do
  marr <- o .:? pack k
  case marr of
    Nothing -> return Nothing
    Just arr -> Just <$> mapM f arr

-- | Wrapper for working with internal arrays with optional elements
_withArraymItem :: Key -> (Object -> Parser (Maybe a)) -> Object -> Parser [a]
_withArraymItem k f o = do
  ma <- _withArrayItem k f o
  return $ fmap fromJust . filter isJust $ ma

----------------------------------------To JSON-------------------------------------
(<:>) :: Convert a => String -> a -> Query
(<:>) key value = [(convert key, jc value)]

infixr 7 <:>

(<:?>) :: Convert a => String -> Maybe a -> Query
(<:?>) key mvalue = case mvalue of
  Nothing -> []
  Just value -> [(convert key, jc value)]

infixr 7 <:?>