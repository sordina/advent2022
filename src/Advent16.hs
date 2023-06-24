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

module Advent16 where

import Text.RawString.QQ (r)
import Text.ParserCombinators.ReadP
    ( ReadP,
      char,
      eof,
      many1,
      readP_to_S,
      satisfy, sepBy1
    )
import Control.Monad (void)
import Data.Char (isSpace, isDigit)
import Control.Applicative ((<|>))
import Utils (crossProductList)
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Control.Monad.State.Strict as State
import Data.Maybe (mapMaybe)
import Debug.Trace (traceShowM)
-- import Debug.Trace (traceShow, traceShowM)
-- import Data.Maybe (fromMaybe)

-- $setup
-- >>> import Test.QuickCheck.All

-- | Testing day16
-- >>> day16 testInput
-- 1651
day16 :: String -> Int
day16 = solution . Map.fromList . parseInput

-- | Testing day16b
-- >>> day16b testInput
-- 1733
day16b :: String -> Int
day16b = solutionB . Map.fromList . parseInput

-- * Types

type Line = (String, (Int, [String]))
type World = Map.Map String (Int, [String])
type State = (String, Set.Set String)
type Table = Map.Map (Int,State) Int

-- * Solution for Part A
-- The approach taken here is to initially write a naive recursive direct solution,
-- then memoize. This can be done via the memocombinators library.

-- | Start at Position "AA", no valves open
solution :: World -> Int
solution w = State.evalState (solve w 30 ("AA", Set.empty)) Map.empty

solve :: World -> Int -> State -> State.State Table Int
solve _ 0 _ = pure 0
solve w t s = do
  g <- State.get
  case Map.lookup (t,s) g of
    Just answer -> do
      pure answer
    Nothing -> do
      xs <- mapM (solve w (pred t)) (step w s)
      let answer = flow w (snd s) + maximum xs
      State.modify (Map.insert (t,s) answer)
      pure answer

step :: World -> State -> [State]
step t (n,o) = open moves
  where
  c = maybe 0 fst (Map.lookup n t)
  moves = maybe [] (map move . snd) (Map.lookup n t)
  move s = (s,o)
  open
    | c < 1          = id
    | Set.member n o = id
    | otherwise      = ((n, Set.insert n o): )

-- Flow determined by open gates times rates.
flow :: World -> Set.Set String -> Int
flow w o = sum $ map snd $ filter (\(n,_c) -> n `Set.member` o) $ Map.toList $ Map.map fst w

-- * Solution for Part B

type StateB = ((String, String), Set.Set String) -- Current positions and open valves
type TableB = Map.Map (Int, StateB) Int -- Map from time and configuration to ultimate volume

-- | Calculate the maximum volume that can flow as a result of possible moves in 26 minutes
-- Does this by recursively solving for all possible moves from the given positions,
-- while memoizing (via State) results to avoid recomputation of any given configuration.
solutionB :: World -> Int
solutionB w = State.evalState (solveB w 26 (("AA", "AA"), Set.empty)) Map.empty

-- | Stateful routine to solve from a given configuration
-- * Checks if configuration has already been solved, returning the solution if so
-- * Otherwise, solves for the maximum of all subsequent moves from the current configuration
--   and adds on the additional flow for the current minute.
solveB :: World -> Int -> StateB -> State.State TableB Int
solveB _ 0 _ = pure 0
solveB w t s = do
  g <- State.get
  case Map.lookup (t,s) g of
    Just answer -> do
      -- traceShowM (answer, t, s)
      pure answer
    Nothing -> do
      -- traceShowM ("MISS!", t, s)
      xs <- mapM (solveB w (pred t)) (stepB w s)
      let answer = flow w (snd s) + maximum xs
      State.modify (Map.insert (t,s) answer)
      pure answer

-- Enumerates state evolutions from current configuration
-- * Finds all the ways that You (n1) and your Elephant (n2) can move together or stay put
stepB :: World -> StateB -> [StateB]
stepB w ((n1,n2), o) = appendO <$> crossProductList moves1 moves2
  where
  moves1 = n1 `stay` maybe [] snd (Map.lookup n1 w)
  moves2 = n2 `stay` maybe [] snd (Map.lookup n2 w)

  -- If you're staying put, you must open a valve.
  -- If you can't do that, then the option is invalid and must be discarded.
  stay n l
    | null l         = pure n -- Stay put if there are no other options otherwise x-product will be null
    | c < 1          = l      -- Don't open valves that with no flow
    | Set.member n o = l      -- Don't open already open valves
    | otherwise      = n : l
    where
    c = maybe 0 fst (Map.lookup n w)

  appendO :: (String, String) -> StateB
  appendO (n1',n2') = (p, n1A <> n2A <> o)
    where
      p   = (min n1' n2', max n1' n2') -- Always order the positions since they are interchangable
      n1A = if n1 == n1' then Set.singleton n1 else Set.empty -- If a position is unchanged that implies that the valve opens
      n2A = if n2 == n2' then Set.singleton n2 else Set.empty

-- * Parser

-- | Testing parseInput
-- >>> parseInput (unlines [head (lines testInput), last (lines testInput)])
-- [("AA",(0,["DD","II","BB"])),("JJ",(21,["II"]))]
--
parseInput :: String -> [Line]
parseInput = map parseLine . lines

parseLine :: String -> Line
parseLine = fst . head  . readP_to_S (line <* eof)

-- Valve JJ has flow rate=21; tunnel leads to valve II
line :: ReadP Line
line = do
  string' "Valve "
  x <- nonSpaces
  string' " has flow rate="
  y <- num
  string' "; "
  z <- multiple <|> single
  pure (x,(y,z))

single :: ReadP [String]
single = do
  string' "tunnel leads to valve "
  pure <$> nonSpaces

multiple :: ReadP [String]
multiple = do
  string' "tunnels lead to valves "
  sepBy1 nonSpaceNonComma (string' ", ")

string' :: String -> ReadP ()
string' = void . string

string :: String -> ReadP ()
string = mapM_ char

nonSpaces :: ReadP String
nonSpaces = many1 (satisfy (not . isSpace))

nonSpaceNonComma :: ReadP String
nonSpaceNonComma = many1 (satisfy (\x -> not (isSpace x) && x /= ','))

num :: (Read a, Num a) => ReadP a
num = read <$> digits
  where
  digit = satisfy isDigit
  digits = many1 digit

testInput :: String
testInput = drop 1 [r|
Valve AA has flow rate=0; tunnels lead to valves DD, II, BB
Valve BB has flow rate=13; tunnels lead to valves CC, AA
Valve CC has flow rate=2; tunnels lead to valves DD, BB
Valve DD has flow rate=20; tunnels lead to valves CC, AA, EE
Valve EE has flow rate=3; tunnels lead to valves FF, DD
Valve FF has flow rate=0; tunnels lead to valves EE, GG
Valve GG has flow rate=0; tunnels lead to valves FF, HH
Valve HH has flow rate=22; tunnel leads to valve GG
Valve II has flow rate=0; tunnels lead to valves AA, JJ
Valve JJ has flow rate=21; tunnel leads to valve II
|]

