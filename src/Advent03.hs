{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE LambdaCase #-}

module Advent03 where

import Data.Set hiding (take, drop, map)
import Data.List.Split (chunksOf)
import Data.Ord
import Data.Char

day3 :: String -> Integer
day3 = sum . map (sum . map (fromIntegral . priority) . common) . lines

common s = toList $ fromList a `intersection` fromList b
  where
  a = take l s
  b = drop l s
  l = length s `div` 2

priority c
  | c >= 'a' && c <= 'z' = 1 + ord c - ord 'a'
  | c >= 'A' && c <= 'Z' = 27 + ord c - ord 'A'

intersections = foldr1 intersection

shared = head . toList . intersections . map fromList

day3b :: String -> Integer
day3b = sum . map (fromIntegral . priority . shared) . chunksOf 3 . lines

