{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}

module Common.Functions where

import Data.Aeson (Options (fieldLabelModifier), defaultOptions)
import Data.Char (toLower)
import Data.List (sort)
import Data.List.Split (splitOn)

-- | Substitution in a template
-- Using: template "Hello {0}, {1}, {0}, {1}," ["Petya", "Vasya"]
template :: String -> [String] -> String
template str args = foldl replaceOneWildCard str $ zip wildCards args
  where
    wildCards = map (\n -> ('{' : show n) ++ "}") [0 :: Int, 1 ..]
    replaceOneWildCard :: String -> (String, String) -> String
    replaceOneWildCard acc (wildCard, arg) =
      let strs = splitOn wildCard acc
       in concat $ safeInit $ foldr (\part acc0 -> part : arg : acc0) [] strs

safeInit :: [a] -> [a]
safeInit [] = []
safeInit a = init a

safeTail :: [a] -> [a]
safeTail [] = []
safeTail a = tail a

-- Check for unique values in list
checkUnique :: (Eq a, Ord a) => [a] -> Bool
checkUnique list = and $ zipWith (/=) sortedList (safeTail sortedList)
  where
    sortedList = sort list

-- For JSON correct parsing
deletePrefixOptions :: Int -> Options
deletePrefixOptions n = defaultOptions {fieldLabelModifier = deletePrefix n}

deletePrefix :: Int -> String -> String
deletePrefix n str = case drop n str of
  x : xs -> toLower x : xs
  [] -> []