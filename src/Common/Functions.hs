{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}

module Common.Functions where

import Control.Monad.IO.Class (MonadIO (..))
import Data.List.Split (splitOn)

-- | Substitution in a template
-- Using: template "Hello {0}, {1}, {0}, {1}," ["Petya", "Vasya"]
template :: String -> [String] -> String
template str args = foldl f str $ zip ts args
  where
    ts = map (\n -> ('{' : show n) ++ "}") [0 :: Int, 1 ..]
    f :: String -> (String, String) -> String
    f acc (t, arg) = replace t arg acc
    replace :: String -> String -> String -> String
    replace t0 s str0 =
      let strs = splitOn t0 str0
       in concat $ init $ foldr (\part acc0 -> part : s : acc0) [] strs

for :: Monad m => m a -> (a -> b) -> m b
for = flip fmap

ifJust :: Monad m => Maybe a -> m () -> m ()
ifJust ma m = case ma of
  Just _ -> m
  _ -> return ()

ifNothing :: Monad m => Maybe a -> m () -> m ()
ifNothing ma m = case ma of
  Nothing -> m
  _ -> return ()

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
safeTail x = tail x

safeInit :: [a] -> [a]
safeInit [] = []
safeInit x = init x

-- Check for unique values in list
checkUnique :: Eq a => [a] -> Bool
checkUnique l = let newList = filter (== True) ((==) <$> l <*> l) in length newList == length l