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
{-# LANGUAGE DataKinds #-}

module Advent15 where

-- $setup
-- >>> import Test.QuickCheck.All

import qualified Data.Set as Set
import qualified Cuboids as Cuboid
import Text.RawString.QQ (r)
import Data.Char (isDigit)
import Control.Monad (void)
import Data.Foldable (find)
import Data.Maybe (fromMaybe, mapMaybe)
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
import Debug.Trace (traceShowId)

-- | Testing day15
-- >>> (solve 10 . parseInput) testInput
-- 26
--
day15 :: String -> Int
day15 = solve 2000000 . parseInput

-- | Testing day15b
-- >>> (solveB 20 . parseInput) testInput
-- 56000011
--
day15b :: String -> Int
day15b = solveB 4000000 . parseInput

-- * Types

type Point = (Int,Int)
type Beacon = (Point, Point)
type Beacons = [Beacon]
type Block = Cuboid.Cuboid 2
type Blocks = Set.Set Block

-- * Solution

solve :: Int -> Beacons -> Int
solve n = Set.size . Set.filter (\(_,y) -> y == n) . Set.unions . map (beacon n)

solveB :: Int -> Beacons -> Int
solveB b
  = frequency
  . fromMaybe (error "Couldn't find a solution")
  . find (bounds b)
  . Set.map cubeCenter
  . Set.filter singleton
  . foldr1 combine
  . map (traceShowId . cubeInvert (cubeBound b) . traceShowId . cubify)

combine :: Blocks -> Blocks -> Blocks
combine a b = traceShowId $ Set.fromList $ mapMaybe (uncurry Cuboid.intersectCuboids) $ Set.toList $ Set.cartesianProduct a b

-- | Testing roundtrip of unCubify . cubify
-- prop> \p -> p == unBasis (basis p)
-- 
basis :: Point -> Point
basis (x,y) = (x+y,y-x)

unBasis :: Point -> Point
unBasis (i,j) = (x,y)
  where
  x = i - (j + i) `div` 2
  y = (j + i) `div` 2

cubify :: Beacon -> Block
cubify (p1@(x,y),p2) = Cuboid.mkVec2 (is,js)
  where
  is      = (bi,ti)
  js      = (bj,tj)
  (bi,bj) = basis (x,y-m)
  (ti,tj) = basis (x,y+m)
  m       = manhattan p1 p2

cubeInvert :: Block -> Block -> Blocks
cubeInvert bs c = Set.delete c $ Set.fromList $ Cuboid.zipWithMVec outside bs c
  where
  outside (bl,bh) (cl,ch) = filter (uncurry (<=)) [(bl,pred cl), (cl,ch), (succ ch,bh)]

cubeBound :: Int -> Block
cubeBound n = Cuboid.mkVec2 ((ax,cx),(ay,cy))
  where
  (ax,ay) = basis (0,0)
  (cx,cy) = basis (n,n)

cuboidUnBasis :: Cuboid.Vec 2 Int -> Point
cuboidUnBasis = unBasis . Cuboid.unVec2

cubeCenter :: Block -> Point
cubeCenter = cuboidUnBasis . Cuboid.center

singleton :: Block -> Bool
singleton = all (uncurry (==))

frequency :: Point -> Int
frequency (x,y) = x * 4000000 + y

beacon :: Int -> Beacon -> Set.Set Point
beacon n (p@(x,_),p') = Set.fromList [(x',y') | x' <- [x-m..x+m], y' <- [n], (x',y') /= p', y' == n, manhattan p (x',y') <= m]
  where
  m = manhattan p p'

bounds :: Int -> Point -> Bool
bounds n (x,y) = x >= 0 && x <= n && y >= 0 && y <= n

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

