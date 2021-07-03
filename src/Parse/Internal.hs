{-# LANGUAGE FlexibleInstances #-}

module Parse.Internal where

import Class (MError)
import Common.Types (Key (..))
import Data.Aeson (FromJSON (parseJSON), Object, Value (Object), (.:), (.:?))
import Data.Aeson.Types (Parser, parseEither)
import Data.Maybe (fromJust, isJust)
import Data.Text (pack)
import qualified Interface.Error.Exports as Error

--All functions in Parser monad are internal

-- Quitting the Parser monad
parseE :: MError m => (Object -> Parser a) -> Object -> m a
parseE f object = Error.catchEither (parseEither f object) Error.ParseError

parseJSONo :: FromJSON a => Object -> Parser a
parseJSONo = parseJSON . Object

-- | Wrapper for working with optional field
mWithItem :: Key -> (Object -> Parser a) -> Object -> Parser (Maybe a)
mWithItem (Key key) f object = do
  mObject1 <- object .:? pack key
  case mObject1 of
    Nothing -> return Nothing
    Just object1 -> Just <$> f object1

-- | Wrapper for working with internal lists
withArrayItem :: Key -> (Object -> Parser a) -> Object -> Parser [a]
withArrayItem (Key key) f object = do
  arr <- object .: pack key
  mapM f arr

-- | Wrapper for working with internal optional lists
mWithArrayItem :: Key -> (Object -> Parser a) -> Object -> Parser (Maybe [a])
mWithArrayItem (Key key) f object = do
  mArr <- object .:? pack key
  case mArr of
    Nothing -> return Nothing
    Just arr -> Just <$> mapM f arr

-- | Wrapper for working with internal arrays with optional elements
withArrayMItem :: Key -> (Object -> Parser (Maybe a)) -> Object -> Parser [a]
withArrayMItem key f object = do
  maList <- withArrayItem key f object
  return $ fmap fromJust . filter isJust $ maList