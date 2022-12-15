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

module Advent14 where

import Debug.Trace
import Text.RawString.QQ (r)
import Data.List.Split (splitOn)
import Utils (fixEq)
import qualified Data.Set as Set

-- | Testing day14
-- >>> day14 testInput
--
day14 :: String -> Int
day14 = pred . length . takeWhileJust . pourSand (500,0) . Set.unions . map rasterize. parseInput

day14b :: String -> Integer
day14b s = error "TODOb"

-- * Types

type Point  = (Int,Int)
type World  = [[Point]]
type Raster = Set.Set Point

-- * Solution

-- | pourSand introduces new grains of sand one at a time and
--   returns where they settle
pourSand :: Point -> Raster -> [Maybe Raster]
pourSand p w = iterate (>>= drip p) (Just w)

drip :: Point -> Raster -> Maybe Raster
drip p w = flip Set.insert w <$> fixEq (>>= fall w) (Just p)

fall :: Raster -> Point -> Maybe Point
fall w p
  | below p w = Just $ move w p
  | otherwise = Nothing

below :: Point -> Raster -> Bool
below (x,y) = any (\(x',y') -> x == x' && y < y')

move :: Raster -> Point -> Point
move w p
  | down  p `notElem` w = down p
  | left  p `notElem` w = left p
  | right p `notElem` w = right p
  | otherwise           = p

down, left, right :: Point -> Point
down (x,y) = (x, succ y)
left (x,y) = (pred x, succ y)
right (x,y) = (succ x, succ y)

-- * Helpers

takeWhileJust :: [Maybe a] -> [a]
takeWhileJust (Just x : xs) = x : takeWhileJust xs
takeWhileJust _ = []

rasterize :: [Point] -> Raster
rasterize l = Set.unions $ zipWith line l (tail l)

line :: Point -> Point -> Set.Set Point
line (x,y) (x',y')
  | x == x' = Set.fromList $ map (x,) [min y y' .. max y y']
  | y == y' = Set.fromList $ map (,y) [min x x' .. max x x']
  | otherwise = error "Couldn't line"

-- * Parser

parseInput :: String -> [[Point]]
parseInput = map parseLine . lines

parseLine :: String -> [Point]
parseLine = map parsePair . splitOn " -> "

parsePair :: String -> Point
parsePair s =
  case splitOn "," s of
    [a,b] -> (read a, read b)
    _ -> error "Couldn't parsePair"

-- | Testing parseInput
-- >>> parseInput testInput
-- [[(498,4),(498,6),(496,6)],[(503,4),(502,4),(502,9),(494,9)]]
testInput :: String
testInput = drop 1 [r|
498,4 -> 498,6 -> 496,6
503,4 -> 502,4 -> 502,9 -> 494,9
|]
