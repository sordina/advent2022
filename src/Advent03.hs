
module Advent03 where

import Data.Set hiding (take, drop, map)
import Data.List.Split (chunksOf)
import Data.Char ( ord, isAsciiUpper, isAsciiLower )

day3 :: String -> Integer
day3 = sum . map (sum . map (fromIntegral . priority) . common) . lines

common :: Ord a => [a] -> [a]
common s = toList $ fromList a `intersection` fromList b
  where
  a = take l s
  b = drop l s
  l = length s `div` 2

priority :: Char -> Int
priority c
  | isAsciiLower c = 1 + ord c - ord 'a'
  | isAsciiUpper c = 27 + ord c - ord 'A'
  | otherwise      = error $ "Couldn't determine priority " <> [c]

intersections :: [Set Char] -> Set Char
intersections = foldr1 intersection

shared :: [String] -> Char
shared = head . toList . intersections . map fromList

day3b :: String -> Integer
day3b = sum . map (fromIntegral . priority . shared) . chunksOf 3 . lines

