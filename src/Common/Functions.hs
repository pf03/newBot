{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}

module Common.Functions where

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

ifJust :: Monad m => Maybe a -> m () -> m ()
ifJust ma m = case ma of
  Just _ -> m
  _ -> return ()

-- Check for unique values in list
checkUnique :: Eq a => [a] -> Bool
checkUnique l = let newList = filter (== True) ((==) <$> l <*> l) in length newList == length l