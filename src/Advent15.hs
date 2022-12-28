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
import Utils (IntX(..))
import Control.Arrow ((***))
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
type Block = Cuboid.CuboidX 2
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
  . map (cubeInvert . cubify)

combine :: Blocks -> Blocks -> Blocks
combine a b = Set.fromList $ mapMaybe (uncurry Cuboid.intersectCuboidsX) $ Set.toList $ Set.cartesianProduct a b

-- | Testing intersection
-- >>> Cuboid.intersectCuboids (Cuboid.mkVec2 ((0,10),(0,10))) (Cuboid.mkVec2 ((5,15),(5,15)))
-- Just V2 <(5,10),(5,10)>
-- >>> Cuboid.intersectCuboids (Cuboid.mkVec2 ((0,10),(0,10))) (Cuboid.mkVec2 ((2,8),(5,15)))
-- Just V2 <(2,8),(5,10)>

-- | Testing roundtrip of unCubify . cubify
-- prop> \p -> p == unBasis (basis p)
-- 
basis :: Point -> Point
basis (x,y) = (x-y,x+y)

unBasis :: Point -> Point
unBasis (i,j) = (x,y)
  where
  y = (j-i) `div` 2
  x = i + ((j-i) `div` 2)
  {-
  i = x-y
  j = x+y
  x = i+y
  y = j-x
  y = j-(i+y)
  2y = j-i
  y = (j-i)/2
  x = i+(j-i)/2
  -}

-- | Testing cubify
-- >>> cubify ((0,0),(10,0))
-- V2 <(-10,10),(-10,10)>
-- >>> cubeCenter (cubify ((0,0),(10,0)))
-- (0,0)
-- >>> cubify ((0,1),(0,0))
-- V2 <(-2,0),(0,2)>
-- >>> cubeCenter (cubify ((0,1),(0,0)))
-- (0,1)
-- >>> cubeCenter (cubify ((0,1),(0,0)))
-- (0,1)

cubify :: Beacon -> Block
cubify (p1@(x,y),p2) = Cuboid.mkVec2 (is,js)
  where
  is      = (Val bi, Val ti)
  js      = (Val bj, Val tj)
  (bi,bj) = basis (x-m,y)
  (ti,tj) = basis (x+m,y)
  m       = manhattan p1 p2

-- | Centers shouldn't move after roundtripping through a cuboid
-- prop> \c@(p,_) -> p == cubeCenter (cubify c)

-- >>> manhattan (0,0) (1,0)
-- 1
-- >>> cubify ((0,0),(1,0))
-- V2 <(-1,1),(-1,1)>
-- >>> cubeCenter (cubify ((0,0),(1,0)))
-- (0,0)

-- | Testing cubeInvert
-- >>> cubeInvert (Cuboid.mkVec2 ((5,6),(5,6)))
-- fromList [V2 <(-Inf,4),(-Inf,4)>,V2 <(-Inf,4),(5,6)>,V2 <(-Inf,4),(7,Inf)>,V2 <(5,6),(-Inf,4)>,V2 <(5,6),(7,Inf)>,V2 <(7,Inf),(-Inf,4)>,V2 <(7,Inf),(5,6)>,V2 <(7,Inf),(7,Inf)>]
cubeInvert :: Block -> Blocks
cubeInvert cube = Set.fromList $ drop 1 {- drops the leading sub cube -}  $ Cuboid.mapMVec regions cube
  where
  regions (l,h) = [(l,h), (NegInf, pred l), (succ h, Inf)]

cuboidUnBasis :: Cuboid.Vec 2 Int -> Point
cuboidUnBasis = unBasis . Cuboid.unVec2

cubeCenter :: Block -> Point
cubeCenter = cuboidUnBasis . Cuboid.center . fmap (finitize *** finitize)

finitize :: IntX Int -> Int
finitize Inf = error "Inf"
finitize NegInf = error "NegInf"
finitize (Val x) = x

singleton :: Block -> Bool
singleton = all (uncurry (==))

frequency :: Point -> Int
frequency (x,y) = x * 4000000 + y

beacon :: Int -> Beacon -> Set.Set Point
beacon n (p@(x,_),p') = Set.fromList [(x',y') | x' <- [x-m..x+m], y' <- [n], (x',y') /= p', manhattan p (x',y') <= m]
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

-- | Checking testInput for partB
-- "Sensor at x=2, y=18: closest beacon is at x=-2, y=15"
-- >>> cubify ((2,18),(-2,15))
-- V2 <(-23,-9),(13,27)>
-- >>> cubeInvert  (cubify ((2,18),(-2,15)))
-- fromList [V2 <(-Inf,-24),(-Inf,12)>,V2 <(-Inf,-24),(13,27)>,V2 <(-Inf,-24),(28,Inf)>,V2 <(-23,-9),(-Inf,12)>,V2 <(-23,-9),(28,Inf)>,V2 <(-8,Inf),(-Inf,12)>,V2 <(-8,Inf),(13,27)>,V2 <(-8,Inf),(28,Inf)>]

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

