{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE NegativeLiterals #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedLabels #-}

module Advent12 where

import qualified Data.Map as Map
import Utils
import Text.RawString.QQ (r)
import Algorithm.Search (dijkstra)
import Data.Char (ord)
import Data.Maybe (mapMaybe)

-- | Testing day12
-- >>> day12 testInput
-- 31
-- 
day12 :: String -> Int
day12 s = maybe -1 fst $ dijkstra step cost end state
  where
  world = parseGrid' id s
  state = fst $ head $ filter (\(_,v) -> 'S' == v) $ Map.toList world
  end k = Map.lookup k world == Just 'E'
  cost _ _ = 1
  step k = [n | Just v <- [Map.lookup k world], n <- adj k, Just v' <- [Map.lookup n world], ord' v' - ord' v <= 1]
  adj (x,y) = [(x-1,y),(x+1,y),(x,y-1),(x,y+1)]
  ord' 'S' = ord 'a'
  ord' 'E' = ord 'z'
  ord'  x  = ord x

day12b :: String -> Integer
day12b s = minimum $ mapMaybe (fmap fst . dijkstra step cost end) states
  where
  world = parseGrid' id s
  states = map fst $ filter (\(_,v) -> v `elem` "Sa") $ Map.toList world
  end k = Map.lookup k world == Just 'E'
  cost _ _ = 1
  step k = [n | Just v <- [Map.lookup k world], n <- adj k, Just v' <- [Map.lookup n world], ord' v' - ord' v <= 1]
  adj (x,y) = [(x-1,y),(x+1,y),(x,y-1),(x,y+1)]
  ord' 'S' = ord 'a'
  ord' 'E' = ord 'z'
  ord'  x  = ord x

-- * Parser

testInput :: String
testInput = drop 1 [r|
Sabqponm
abcryxxl
accszExk
acctuvwj
abdefghi
|]
