{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Advent04 where

import Debug.Trace
import Data.Char
import Control.Arrow
import Control.Monad
import Control.Applicative hiding (many)
import Text.ParserCombinators.ReadP

day4 :: String -> Int
day4 = length . filter (subsumption . parse) . lines

example = "63-93,64-94"

subsumption ((a,b),(c,d)) = (a <= c && b >= d) || (a >= c && b <= d)

digit = satisfy isDigit
digits = many1 digit
range :: ReadP (Int,Int) = do
  a <- digits
  void $ char '-'
  b <- digits
  pure (read a, read b)
pair = do
  r1 <- range
  void $ char ','
  r2 <- range
  eof
  pure (r1,r2)
parse s = fst $ head $ readP_to_S pair s


day4b :: String -> Int
day4b = length . filter (overlap . parse) . lines

overlap ((a,b),(c,d)) = (a <= c && b >= c) || (a >= c && a <= d)
