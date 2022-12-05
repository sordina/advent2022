{-# LANGUAGE TypeApplications #-}

module Advent01 where

import Data.List.Split

day1 :: String -> Integer
day1 = maximum . map (sum . map (read @Integer)) . splitOn [""] . lines

-- | Testing against test_input
--
-- >>> day1 test_input

test_input :: String
test_input = "123\n456\n\n789\n1234"

