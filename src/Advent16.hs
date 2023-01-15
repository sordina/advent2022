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
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.MemoCombinators as Memo
import Debug.Trace (traceShow)

-- $setup
-- >>> import Test.QuickCheck.All

timeLimit :: Integer
timeLimit = 30

-- | Testing day16
-- >>> day16 testInput
-- 1651
day16 :: String -> Int
day16 = solution . Map.fromList . parseInput

-- | Testing day16b
-- >>> day16b testInput
--
day16b :: String -> Int
day16b = error "TODOb"

-- * Types

type Line = (String, (Int, [String]))
type World = Map.Map String (Int, [String])
type State = (String, Set.Set String)

-- * Solution
-- The approach taken here is to initially write a naive recursive direct solution,
-- then memoize. This can be done via the memocombinators library.

solution :: World -> Int
solution w = solve w 30 ("AA", Set.empty)

solve :: World -> Int -> State -> Int
solve w = Memo.memo2 memoRemaining memoState solve'
  where
  solve' :: Int -> State -> Int
  solve' 0 _ = 0
  solve' t s = traceShow (t, s) $ flow w s + maximum (take 2 (map (solve w (pred t)) (step w s)))

memoRemaining :: Memo.Memo Int
memoRemaining = Memo.unsafeArrayRange (0,30)

memoState :: Memo.Memo State
memoState = Memo.pair memoName memoSet

-- "XY" string
memoName :: Memo.Memo String
memoName = Memo.list Memo.char

memoSet :: Memo.Memo (Set.Set String)
memoSet = Memo.wrap Set.fromAscList Set.toAscList (Memo.list memoName)

step :: World -> State -> [State]
step t (n,o) = open moves
  where
  moves = maybe [] (map move . snd) (Map.lookup n t)
  move s = (s,o)
  open
    | Set.member n o = id
    | otherwise      = ((n, Set.insert n o): )

-- Flow determined by open gates times rates.
flow :: World -> State -> Int
flow w (_n,o) = sum $ map snd $ filter (\(n,_c) -> n `Set.member` o) $ Map.toList $ Map.map fst w

-- * Parser

-- | Start at Position "AA", no valves open

-- | Testing parseInput
-- >>> parseInput (unlines [head (lines testInput), last (lines testInput)])
-- [("AA",0,["DD","II","BB"]),("JJ",21,["II"])]
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
