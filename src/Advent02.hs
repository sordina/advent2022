{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE LambdaCase #-}

module Advent02 where

import Data.List
import Data.List.Split

day2 :: String -> Integer
day2 = sum . map score . lines

score (l:' ':r:_) = value r + points l r

points 'A' 'X' = 3
points 'B' 'Y' = 3
points 'C' 'Z' = 3
points 'A' 'Y' = 6
points 'A' 'Z' = 0
points 'B' 'X' = 0
points 'B' 'Z' = 6
points 'C' 'X' = 6
points 'C' 'Y' = 0
points x y = error $ "Unknown moves " <> [x] <> " " <> [y]

value = \case
  'X' -> 1
  'Y' -> 2
  'Z' -> 3
  x   -> error $ "Unknown move " <> [x]

day2b :: String -> Integer
day2b = sum . map score . map move . lines

move (l:' ':r:_) = [l,' ',mine]
  where
  mine = case (l,r) of
    ('A','X') -> 'Z'
    ('A','Y') -> 'X'
    ('A','Z') -> 'Y'
    ('B','X') -> 'X'
    ('B','Y') -> 'Y'
    ('B','Z') -> 'Z'
    ('C','X') -> 'Y'
    ('C','Y') -> 'Z'
    ('C','Z') -> 'X'
    (x,y)     -> error $ "Unknown suggestion " <> [x] <> " " <> [y]

-- | Testing against test_input
--
-- >>> day2 test_input

test_input :: String
test_input = "123\n456\n\n789\n1234"

