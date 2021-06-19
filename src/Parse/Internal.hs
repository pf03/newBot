{-# LANGUAGE FlexibleInstances #-}

module Parse.Internal where

import Common.Types ( Key(..) )
import Data.Aeson ( (.:), (.:?), FromJSON(parseJSON), Object, Value(Object) )
import Data.Aeson.Types (Parser, parseEither)
import Data.Maybe (fromJust, isJust)
import Data.Text (pack)
import Class (MError)
import qualified Interface.Error.Exports as Error

--All functions in Parser monad are internal

-- Quitting the Parser monad
parseE :: MError m => (Object -> Parser a) -> Object -> m a
parseE f object = Error.catchEither (parseEither f object) Error.ParseError

parseJSONo :: FromJSON a => Object -> Parser a
parseJSONo = parseJSON . Object

-- | Wrapper for working with optional field
mwithItem :: Key -> (Object -> Parser a) -> Object -> Parser (Maybe a)
mwithItem (Key key) f object = do
  mobject1 <- object .:? pack key
  case mobject1 of
    Nothing -> return Nothing
    Just object1 -> Just <$> f object1

-- | Wrapper for working with internal lists
withArrayItem :: Key -> (Object -> Parser a) -> Object -> Parser [a]
withArrayItem (Key key) f object = do
  arr <- object .: pack key
  mapM f arr

-- | Wrapper for working with internal optional lists
mwithArrayItem :: Key -> (Object -> Parser a) -> Object -> Parser (Maybe [a])
mwithArrayItem (Key key) f object = do
  marr <- object .:? pack key
  case marr of
    Nothing -> return Nothing
    Just arr -> Just <$> mapM f arr

-- | Wrapper for working with internal arrays with optional elements
withArraymItem :: Key -> (Object -> Parser (Maybe a)) -> Object -> Parser [a]
withArraymItem key f object = do
  maList <- withArrayItem key f object
  return $ fmap fromJust . filter isJust $ maList