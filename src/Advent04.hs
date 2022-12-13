{-# LANGUAGE ScopedTypeVariables #-}

module Advent04 where

import Data.Char ( isDigit )
import Control.Monad ( void )
import Text.ParserCombinators.ReadP
    ( ReadP, char, eof, many1, readP_to_S, satisfy )

day4 :: String -> Int
day4 = length . filter (subsumption . parse) . lines

example :: String
example = "63-93,64-94"

subsumption :: (Ord a1, Ord a2) => ((a1, a2), (a1, a2)) -> Bool
subsumption ((a,b),(c,d)) = (a <= c && b >= d) || (a >= c && b <= d)

digit :: ReadP Char
digit = satisfy isDigit

digits :: ReadP [Char]
digits = many1 digit

range :: ReadP (Int, Int)
range :: ReadP (Int,Int) = do
  a <- digits
  void $ char '-'
  b <- digits
  pure (read a, read b)

pair :: ReadP ((Int, Int), (Int, Int))
pair = do
  r1 <- range
  void $ char ','
  r2 <- range
  eof
  pure (r1,r2)

parse :: String -> ((Int, Int), (Int, Int))
parse s = fst $ head $ readP_to_S pair s


day4b :: String -> Int
day4b = length . filter (overlap . parse) . lines

overlap :: Ord a => ((a, a), (a, a)) -> Bool
overlap ((a,b),(c,d)) = (a <= c && b >= c) || (a >= c && a <= d)
