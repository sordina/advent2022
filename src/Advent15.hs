{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE NegativeLiterals #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE InstanceSigs #-}

module Advent15 where

import Text.RawString.QQ (r)
import qualified Data.Set as Set
import Data.Char (isDigit)
import Control.Monad (void)
import Text.ParserCombinators.ReadP
    ( ReadP,
      char,
      eof,
      look,
      many1,
      pfail,
      readP_to_S,
      satisfy
    )

-- | Testing day15
-- >>> (solve 10 . parseInput) testInput
-- 26
--
day15 :: String -> Int
day15 = solve 2000000 . parseInput

day15b :: String -> Int
day15b = error "TODOb"

-- * Types

type Point = (Int,Int)
type Beacon = (Point, Point)
type Beacons = [Beacon]

-- * Solution

solve :: Int -> Beacons -> Int
solve n = Set.size . Set.filter (\(_,y) -> y == n) . Set.unions . map (beacon n)

beacon :: Int -> Beacon -> Set.Set Point
beacon n (p@(x,_),p') = Set.fromList [(x',y') | x' <- [x-m..x+m], y' <- [n], (x',y') /= p', y' == n, manhattan p (x',y') <= m]
  where
  m = manhattan p p'

manhattan :: Point -> Point -> Int
manhattan (x,y) (x',y') = abs (x-x') + abs (y-y')

-- * Helpers

-- * Parser

-- | Testing parseInput
-- >>> parseInput (head (lines testInput))
-- [((2,18),(-2,15))]
--
parseInput :: String -> Beacons
parseInput = map parseLine . lines

parseLine :: String -> ((Int,Int),(Int,Int))
parseLine = fst . head  . readP_to_S (line <* eof)

line :: ReadP Beacon
line = do
  void $ string "Sensor at x="
  x <- signed
  void $ string ", y="
  y <- signed
  void $ string ": closest beacon is at x="
  x' <- signed
  void $ string ", y="
  y' <- signed
  pure ((x,y), (x',y'))

string :: String -> ReadP ()
string = mapM_ char

signed ::  (Read a, Num a) => ReadP a
signed = do
  c <- peek
  if c == '-'
     then char '-' *> (negate <$> num)
     else num

peek :: ReadP Char
peek = do
  remaining <- look
  case remaining of
    (h:_) -> pure h
    [] -> pfail

num :: (Read a, Num a) => ReadP a
num = read <$> digits
  where
  digit = satisfy isDigit
  digits = many1 digit


testInput :: String
testInput = drop 1 [r|
Sensor at x=2, y=18: closest beacon is at x=-2, y=15
Sensor at x=9, y=16: closest beacon is at x=10, y=16
Sensor at x=13, y=2: closest beacon is at x=15, y=3
Sensor at x=12, y=14: closest beacon is at x=10, y=16
Sensor at x=10, y=20: closest beacon is at x=10, y=16
Sensor at x=14, y=17: closest beacon is at x=10, y=16
Sensor at x=8, y=7: closest beacon is at x=2, y=10
Sensor at x=2, y=0: closest beacon is at x=2, y=10
Sensor at x=0, y=11: closest beacon is at x=2, y=10
Sensor at x=20, y=14: closest beacon is at x=25, y=17
Sensor at x=17, y=20: closest beacon is at x=21, y=22
Sensor at x=16, y=7: closest beacon is at x=15, y=3
Sensor at x=14, y=3: closest beacon is at x=15, y=3
Sensor at x=20, y=1: closest beacon is at x=15, y=3
|]

