{-# LANGUAGE FlexibleInstances #-}

module Logic.Parse.Internal where

import Common.Types ( Key ) 
import Data.Aeson ( (.:), (.:?), FromJSON(parseJSON), Object, Value(Object) )
import Data.Aeson.Types (Parser, parseEither)
import Data.Maybe (fromJust, isJust)
import Data.Text (pack)
import Interface.Class (MError)
import qualified Interface.MError.Exports as Error

--All functions in Parser monad are internal

-- Quitting the Parser monad
parseE :: MError m => (Object -> Parser a) -> Object -> m a
parseE f o = Error.catchEither (parseEither f o) Error.ParseError

parseJSONo :: FromJSON a => Object -> Parser a
parseJSONo = parseJSON . Object

-- | Wrapper for working with optional field
mwithItem :: Key -> (Object -> Parser a) -> Object -> Parser (Maybe a)
mwithItem k f o = do
  mo <- o .:? pack k
  case mo of
    Nothing -> return Nothing
    Just o1 -> Just <$> f o1

-- | Wrapper for working with internal lists
withArrayItem :: Key -> (Object -> Parser a) -> Object -> Parser [a]
withArrayItem k f o = do
  arr <- o .: pack k
  mapM f arr

-- | Wrapper for working with internal optional lists
mwithArrayItem :: Key -> (Object -> Parser a) -> Object -> Parser (Maybe [a])
mwithArrayItem k f o = do
  marr <- o .:? pack k
  case marr of
    Nothing -> return Nothing
    Just arr -> Just <$> mapM f arr

-- | Wrapper for working with internal arrays with optional elements
withArraymItem :: Key -> (Object -> Parser (Maybe a)) -> Object -> Parser [a]
withArraymItem k f o = do
  ma <- withArrayItem k f o
  return $ fmap fromJust . filter isJust $ ma