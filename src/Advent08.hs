{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE QuasiQuotes #-}

module Advent08 where

import Control.Arrow (first, second)
import Utils ( parseGrid' )
import Data.List (unfoldr)
import qualified Data.Map as Map
import Text.RawString.QQ (r)

-- | Testing day8
-- >>> day8 testInput
-- 21

testInput :: String
testInput = drop 1 [r|
30373
25512
65332
33549
35390
|]

day8 :: String -> Int
day8 s = length $ filter id $ Map.elems $ Map.mapWithKey f g
  where
  g   = parseGrid' (read . return) s
  u w = drop 1 . unfoldr (\b -> (, w b) <$> Map.lookup b g)
  f :: (Int,Int) -> Int -> Bool
  f k v
    =  all (<v) (u (first pred) k)
    || all (<v) (u (first succ) k)
    || all (<v) (u (second pred) k)
    || all (<v) (u (second succ) k)

-- * Part B
--
-- | Testing day8b
-- >>> day8b testInput
-- 8

day8b :: String -> Int
day8b s = maximum $ Map.elems $ Map.mapWithKey f g
  where
  g   = parseGrid' (read . return) s
  u w = drop 1 . unfoldr (\b -> (, w b) <$> Map.lookup b g)
  f :: (Int,Int) -> Int -> Int
  f k v
    = length (takeUntil (>=v) (u (first  pred) k))
    * length (takeUntil (>=v) (u (first  succ) k))
    * length (takeUntil (>=v) (u (second pred) k))
    * length (takeUntil (>=v) (u (second succ) k))

takeUntil :: (a -> Bool) -> [a] -> [a]
takeUntil f (x:xs)
  | f x = [x]
  | otherwise = x : takeUntil f xs
takeUntil _ _ = []

