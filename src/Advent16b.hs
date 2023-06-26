{-# LANGUAGE InstanceSigs #-}
module Advent16b where

-- Translated from https://www.reddit.com/r/adventofcode/comments/zn6k1l/comment/j2xhog7/?utm_source=share&utm_medium=web2x&context=3
-- I don't fully understand this solution, but comments are added to outline the broad steps below.

import Advent16 (parseInput)
import Data.Maybe (fromMaybe, fromJust)
import Data.Bits (Bits(..))
import Data.Foldable (for_)
import Control.Monad (unless)

import qualified Data.Map as Map
import qualified Control.Monad.State as State
import qualified Control.Lens as Lens

day16b2 :: String -> Int
day16b2 = solve . parseInput

-- The solution (pre) answer is a mapping from a valve configuration state (represented as a bitmap of open valves) to volume
-- Bitmaps are used due to the convenience of checking independence of solutions for two parties (you and an elephant).
-- If a mutually exclusive set of valves are toggled for you and the elephant then this is a candidate for an optimal solution.
type Answer = Map.Map Int Int

-- Infinite extension of numbers is introduced for tropical reasons
data Infinite a = Finite a | Infinity

infinadd :: Num a => Infinite a -> Infinite a -> Infinite a
infinadd (Finite a) (Finite b) = Finite (a + b)
infinadd _ _ = Infinity

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
    Lens.at (i,j) Lens..= getInfinite (min a (b `infinadd` c))

-- Finds the maximum volume that can be produced in 26 steps with you and an elephant.
solve :: [(String, (Int, [String]))] -> Int
solve puzzle =
  let
    directory = Map.fromList puzzle -- A convenient lookup from node to associated info
    graph = Map.map snd directory -- Adjacency information
    flows = [(k,v) | (k,(v,_)) <- puzzle, v > 0] -- All positive flows
    indicies = Map.fromList $ zipWith index [0..] puzzle -- Mapping from node id to bitmap representing the node
    keys = Map.keys graph -- Convenience list of all the node ids
    -- distances are initially set to 1 if adjacent, omitted if not
    distances =  Map.fromList [((f,t),1) | (f,(_, ts)) <- puzzle, t <- ts]
    -- The floyd algorithm is then used to set the distances to minimum traversal times
    distances' = floyd keys distances

    -- Answer map is updated recursively via the volume argument
    visit :: String -> Int -> Int -> Int -> State.State Answer ()
    visit valve minutes bitmap volume = do
      -- Set the memoized value for the current valve to the max of the volume calculated when visiting,
      -- and its previously memoized value
      a <- State.gets (fromMaybe 0 . Map.lookup bitmap)
      Lens.at bitmap Lens..= Just (max a volume)
      -- Try moving to valves that have positive flow - Recursion bottoms out due to memoization `unless`.
      for_ flows \(valve2, flow) -> do
        let
          d = fromJust $ Map.lookup (valve, valve2) distances'
          remainingMinutes = minutes - d - 1 -- Use floyd minutes map to deterime how much time will have passed
          iv2 = fromJust $ Map.lookup valve2 indicies -- Find the bitmap for the new valve
        -- Only recurse if there is time remaining and the candidate valve isn't already open
        unless (remainingMinutes < 1 || (iv2 .&. bitmap) /= 0) do
          -- When visiting a subsequent node we know that
          -- * The reason to visit it is to open its valve since transitive visits are not performed
          -- * The additional volume generated will be its flow * remainingMinutes
          -- * So the new volume passed in will be the prior volume plus the new valve accounting
          visit valve2 remainingMinutes (bitmap .|. iv2) (volume + flow * remainingMinutes)

    visited2 = Map.empty Lens.&~ visit "AA" 26 0 0
    part2 = maximum [v1 + v2 |
        (bitm1, v1) <- Map.toList visited2,
        (bitm2, v2) <- Map.toList visited2,
        (bitm1 .&. bitm2) == 0
      ]

  in part2

index :: Int -> (String, a) -> (String, Int)
index i (v, _) = (v, shiftL 1 i)
