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

floyd :: Ord a => [a] -> Map.Map (a,a) Int -> Map.Map (a,a) Int
floyd keys = State.execState do
  for_ [(k,i,j) | k <- keys, i <- keys, j <- keys] \(k,i,j) -> do
    a <- State.gets (Map.lookup (i,j))
    b <- State.gets (Map.lookup (i,k))
    c <- State.gets (Map.lookup (k,j))
    Lens.at (i,j) Lens..=
      do
        a' <- a
        b' <- b
        c' <- c
        pure $ min a' (b' + c')

solve :: [(String, (Int, [String]))] -> Int
solve puzzle =
  let
    directory = Map.fromList puzzle
    graph = Map.map snd directory
    flows = Map.filter (>0) $ Map.map fst directory
    indicies = Map.fromList $ zipWith index [0..] puzzle
    keys = Map.keys graph
    distances = Map.fromList [ ((v,l), fromMaybe 1000 (bool Nothing (Just 1) . (l `elem`) =<< Map.lookup v graph)) | l <- keys, v <- keys ]
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
