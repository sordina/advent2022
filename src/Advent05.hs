{-# LANGUAGE ScopedTypeVariables #-}

module Advent05 where

import Data.Char ( isDigit, isSpace )
import Data.List ( foldl', transpose )
import Data.Map (Map)
import qualified Data.Map as Map
import Data.List.Split ( splitOn )
import Control.Arrow ( Arrow((&&&)) )
import Control.Lens ( (&), _Just, (%~), At(at) )
import Control.Monad ( void )
import Text.ParserCombinators.ReadP
    ( ReadP(), char, eof, many1, readP_to_S, satisfy, string )

type State = Map Int String

day5 :: String -> String
day5 = map (head . snd) . Map.toAscList . interpret . (parseState . head &&& map parseInstruction . last) . splitOn [[]] . lines

parseState :: [String] -> State
parseState = Map.fromList . map (read.take 1 &&& reverse . filter (not . isSpace) . tail) . filter (\x -> not (null x) && isDigit (head x)) . transpose . reverse

digit :: ReadP Char
digit  = satisfy isDigit

digits :: ReadP [Char]
digits = many1 digit

spaces :: ReadP [Char]
spaces = many1 (char ' ')

instruction :: ReadP (Int, Int, Int)
instruction = do
  void $ string "move "
  n <- digits <* spaces
  void $ string "from "
  a <- digits <* spaces
  void $ string "to "
  b <- digits
  eof
  pure (read n, read a, read b)

parseInstruction :: String -> (Int,Int,Int)
parseInstruction = fst . head . readP_to_S instruction

example :: String
example = "move 5 from 1 to 8"

interpret :: (State, [(Int,Int,Int)]) -> State
interpret (s,is) = foldl' move s unrolled
  where
  unrolled = is >>= (\(n,x,y) -> replicate n (x, y))

move :: State -> (Int, Int) -> State
move is (x,y) = placed
  where
  item :: Char
  item    = maybe (error "Map error") head $ Map.lookup x is
  plucked = is & at x . _Just %~ tail
  placed  = plucked & at y . _Just %~ (item:)

day5b :: String -> String
day5b = map (head . snd) . Map.toAscList . interpret2 . (parseState . head &&& map parseInstruction . last) . splitOn [[]] . lines

interpret2 :: (State, [(Int,Int,Int)]) -> State
interpret2 (s,is) = foldl' move2 s is

move2 :: State -> (Int, Int, Int) -> State
move2 is (n,x,y) = placed
  where
  items :: String
  items = maybe (error "Map error") (take n) $ Map.lookup x is
  plucked = is & at x . _Just %~ drop n
  placed  = plucked & at y . _Just %~ (items++)
