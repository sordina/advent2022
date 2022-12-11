{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE NegativeLiterals #-}

module Advent10 where

import Utils
import Text.RawString.QQ (r)

data Instruction
  = Nop
  | Add Integer
  deriving (Eq, Show)

-- | Testing day10
-- >>> last (registers testInput)
-- -1

-- | Testing day10
-- >>> day10 testInput2
-- 13140

day10 :: String -> Integer
day10 s = sum $ map (\x-> fromIntegral x * (solution !! pred x)) significantCycles
  where
  solution = {- traceWithId (unlines . map show . zip [1..]) $ -} registers s

registers :: String -> [Integer]
registers s = concat $ scanl interpret [1] parsed
  where
  parsed = map (parseInput . words) (lines s)

interpret :: [Integer] -> Instruction -> [Integer]
interpret ns Nop = [last ns]
interpret ns (Add n) = [last ns, n + last ns]

parseInput :: [String] -> Instruction
parseInput ["noop"] = Nop
parseInput ["addx", n] = Add (read n)

day10b :: String -> Int
day10b s = error "TODOb"

significantCycles :: [Int]
significantCycles = [ 20, 60, 100, 140, 180, 220 ]

-- Test Data

testInput :: String
testInput = drop 1 [r|
noop
addx 3
addx -5
|]

testInput2 :: String
testInput2 = drop 1 [r|
addx 15
addx -11
addx 6
addx -3
addx 5
addx -1
addx -8
addx 13
addx 4
noop
addx -1
addx 5
addx -1
addx 5
addx -1
addx 5
addx -1
addx 5
addx -1
addx -35
addx 1
addx 24
addx -19
addx 1
addx 16
addx -11
noop
noop
addx 21
addx -15
noop
noop
addx -3
addx 9
addx 1
addx -3
addx 8
addx 1
addx 5
noop
noop
noop
noop
noop
addx -36
noop
addx 1
addx 7
noop
noop
noop
addx 2
addx 6
noop
noop
noop
noop
noop
addx 1
noop
noop
addx 7
addx 1
noop
addx -13
addx 13
addx 7
noop
addx 1
addx -33
noop
noop
noop
addx 2
noop
noop
noop
addx 8
noop
addx -1
addx 2
addx 1
noop
addx 17
addx -9
addx 1
addx 1
addx -3
addx 11
noop
noop
addx 1
noop
addx 1
noop
noop
addx -13
addx -19
addx 1
addx 3
addx 26
addx -30
addx 12
addx -1
addx 3
addx 1
noop
noop
noop
addx -9
addx 18
addx 1
addx 2
noop
noop
addx 9
noop
noop
noop
addx -1
addx 2
addx -37
addx 1
addx 3
noop
addx 15
addx -21
addx 22
addx -6
addx 1
noop
addx 2
addx 1
noop
addx -10
noop
noop
addx 20
addx 1
addx 2
addx 2
addx -6
addx -11
noop
noop
noop
|]
