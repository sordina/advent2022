{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Advent06 where

import Debug.Trace
import Control.Arrow
import Data.List
import qualified Data.Set as Set

day6 :: String -> Int
day6 = (+4) . length . fst . head . dropWhile (not . (==4) . Set.size . Set.fromList . take 4 . snd) . splits

splits [] = [([],[])]
splits l@(x:xs) = ([],l) : map (first (x:)) (splits xs)

day6b :: String -> Int
day6b = (+14) . length . fst . head . dropWhile (not . (==14) . Set.size . Set.fromList . take 14 . snd) . splits

