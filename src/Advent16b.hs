{-# LANGUAGE InstanceSigs #-}
module Advent16b where

-- Translated from https://www.reddit.com/r/adventofcode/comments/zn6k1l/comment/j2xhog7/?utm_source=share&utm_medium=web2x&context=3
-- I don't fully understand this solution, but comments are added to outline the broad steps below.

import Advent16 (parseInput)
import Data.Maybe (fromMaybe, fromJust)
import Data.Bits (Bits(..))
import Data.Foldable (for_)
import Control.Monad (unless)
import Data.Bool (bool)

import qualified Data.Map as Map
import qualified Control.Monad.State as State
import qualified Control.Lens as Lens

day16b2 :: String -> Int
day16b2 = solve . parseInput

-- The solution (pre) answer is a mapping from a valve configuration state (represented as a bitmap of open valves) to volume
-- Bitmaps are used due to the convenience of checking independence of solutions for two parties (you and an elephant).
-- If a mutually exclusive set of valves are toggled for you and the elephant then this is a candidate for an optimal solution.
type Answer = Map.Map Int Int

data Infinite a = Finite a | Infinity

instance Num a => Num (Infinite a) where
  (Finite a) + (Finite b) = Finite (a+b)
  _ + _ = Infinity
  fromInteger x = Finite (fromInteger x)

instance Eq a => Eq (Infinite a) where
  Infinity == Infinity = True
  (Finite a) == (Finite b) = a == b
  _ == _ = False

instance Ord a => Ord (Infinite a) where
  compare :: Ord a => Infinite a -> Infinite a -> Ordering
  compare Infinity Infinity = EQ
  compare (Finite _) Infinity = LT
  compare Infinity (Finite _) = GT
  compare (Finite a) (Finite b) = a `compare` b

mkInfinite :: Num a => Maybe a -> Infinite a
mkInfinite Nothing = Infinity
mkInfinite (Just a) = Finite a

getInfinite :: Num a => Infinite a -> Maybe a
getInfinite Infinity = Nothing
getInfinite (Finite a) = Just a

-- Floyd-warshall algoritm for minimum distance between nodes in a graph
-- This is used to create a distance matrix that can skip traversal steps when recursing to all potential subsequent nodes.
-- "Infinite" extension of numbers is used to allow min to work without default values (initially omitting non-adjacent values from the state)
floyd :: Ord a => [a] -> Map.Map (a,a) Int -> Map.Map (a,a) Int
floyd keys = State.execState do
  for_ [(k,i,j) | k <- keys, i <- keys, j <- keys] \(k,i,j) -> do
    a <- State.gets (mkInfinite . Map.lookup (i,j))
    b <- State.gets (mkInfinite . Map.lookup (i,k))
    c <- State.gets (mkInfinite . Map.lookup (k,j))
    Lens.at (i,j) Lens..= getInfinite (min a (b + c))

-- Finds the maximum volume that can be produced in 26 steps with you and an elephant.
solve :: [(String, (Int, [String]))] -> Int
solve puzzle =
  let
    directory = Map.fromList puzzle -- A convenient lookup from node to associated info
    graph = Map.map snd directory -- Adjacency information
    flows = Map.filter (>0) $ Map.map fst directory -- Flow information
    indicies = Map.fromList $ zipWith index [0..] puzzle -- Mapping from node id to bitmask representing the node
    keys = Map.keys graph -- Convenience list of all the node ids
    -- distances are initially set to 1 if adjacent, omitted if not
    distances =  Map.fromList [((f,t),1) | (f,(_, ts)) <- puzzle, t <- ts]
    -- The floyd algorithm is then used to set the distances to minimum traversal times
    distances' = floyd keys distances

    visit :: String -> Int -> Int -> Int -> State.State Answer ()
    visit valve minutes bitmask pressure = do
      a <- State.gets (fromMaybe 0 . Map.lookup bitmask)
      Lens.at bitmask Lens..= Just (max a pressure)
      for_ (Map.toList flows) \(valve2, flow) -> do
        let
          d = fromJust $ Map.lookup (valve, valve2) distances'
          remainingMinutes = minutes - d - 1
          iv2 = fromJust $ Map.lookup valve2 indicies
        unless ((iv2 .&. bitmask) /= 0 || remainingMinutes < 1) do
          visit valve2 remainingMinutes (bitmask .|. iv2) (pressure + flow * remainingMinutes)

    visited2 = Map.empty Lens.&~ visit "AA" 26 0 0
    part2 = maximum [v1 + v2 |
        (bitm1, v1) <- Map.toList visited2,
        (bitm2, v2) <- Map.toList visited2,
        (bitm1 .&. bitm2) == 0
      ]

  in part2

index :: Int -> (String, a) -> (String, Int)
index i (v, _) = (v, shiftL 1 i)
