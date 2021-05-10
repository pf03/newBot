{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE UndecidableInstances #-}
module Common.Misc where

import           Control.Monad.IO.Class
import           Data.Aeson
import qualified Data.ByteString.Char8      as BC
import qualified Data.ByteString.Lazy.Char8 as LC
import           Data.List.Split
import           Data.Text                  (pack)
import           Data.Text.Encoding

-----------------------------Types---------------------------------------------
type BS = BC.ByteString
type LBS = LC.ByteString
type Path = String
type Message = String
type Label = String
data Command = Help | Repeat | Start | Unknown String| Button Int deriving (Show, Eq)
type UpdateId = Int
type ChatId = Int
type Key = String
type UserId = Int
type IntId = Int
type StrId = String
type FileId = String
type Url = String
type ItemName = String
type TimeOut = Int   -- time out long polling

-----------------------------Template------------------------------------------
-- | Substitution in a template
-- Using: template "Hello {0}, {1}, {0}, {1}," ["Petya", "Vasya"]
template :: String -> [String] -> String
template str args = foldl f str $ zip ts args where
    ts = map (\n -> ('{':show n)++"}")[0::Int,1..]
    f:: String -> (String, String) -> String
    f acc (t, arg) = replace t arg acc
    replace :: String -> String -> String -> String
    replace t0 s str0 = let strs = splitOn t0 str0 in
        concat $ init $ foldr (\part acc0 -> part:s:acc0) [] strs

-----------------------------Monadic and simple functions----------------------
ifJust :: Monad m => Maybe a -> m () -> m ()
ifJust ma m = case ma of
      Just _ -> m
      _      -> return ()

ifNothing :: Monad m => Maybe a -> m () -> m ()
ifNothing ma m = case ma of
      Nothing -> m
      _       -> return ()

printT :: (MonadIO m, Show a) => a -> m ()
printT = liftIO . print

putStrLnT :: MonadIO m => String -> m ()
putStrLnT = liftIO . putStrLn

maybeM :: (Monad m) => m (Maybe a) -> m b -> (a -> m b) -> m b
maybeM mma mb k = do
    ma <- mma
    maybe mb k ma

safeTail :: [a] -> [a]
safeTail [] = []
safeTail x  = tail x

safeInit :: [a] -> [a]
safeInit [] = []
safeInit x  = init x

----------------------------------------Convert--------------------------------------------------
class Convert a where
    convert :: a -> BC.ByteString

instance Convert String where
    convert = encodeUtf8 . pack  -- encodeUtf8 for correct cyrillic encoding

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

jc :: Convert a => a -> Maybe BC.ByteString
jc = Just . convert
