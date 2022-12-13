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

module Advent13 where

import Text.RawString.QQ (r)
import Data.Char (isDigit)
import Text.ParserCombinators.ReadP hiding (optional, get)
import Data.List.Split (chunksOf)
import Data.List (sort)

data Tree a = Leaf a | Branches [Tree a]
  deriving Eq

instance Show a => Show (Tree a) where
  show :: Show a => Tree a -> String
  show (Leaf x) = show x
  show (Branches xs) = show xs

instance Ord a => Ord (Tree a) where
  compare :: Ord a => Tree a -> Tree a -> Ordering
  a `compare` b = a `check` b

-- | Testing day13
-- >>> day13 testInput
-- 
day13 :: String -> Int
day13 = sum . map fst . filter snd . zip [1..] . map ((== LT) . uncurry check) . parseInput

day13b :: String -> Integer
day13b s = product $ map snd $ filter (isDecoder . fst) $ flip zip [1..] $ sort trees
  where
  trees = parseInputList (s <> "\n" <> decoder2 <> "\n" <> decoder6)
  isDecoder t = show t `elem` [decoder2, decoder6]
  decoder2 = "[[2]]"
  decoder6 = "[[6]]"

-- * Solution

check :: Ord a => Tree a -> Tree a -> Ordering
check x@(Leaf _)      y@(Branches _)      = check (Branches [x]) y
check x@(Branches _)  y@(Leaf _)          = check x (Branches [y])
check (Leaf x)          (Leaf y)          = x `compare` y
check (Branches [])     (Branches  [])    = EQ
check (Branches [])     (Branches _ys)    = LT
check (Branches _xs)    (Branches [])     = GT
check (Branches (x:xs)) (Branches (y:ys)) =
  case check x y of
    EQ -> check (Branches xs) (Branches ys)
    c  -> c

-- * Parser

parseInput :: String -> [(Tree Integer, Tree Integer)]
parseInput = map pair . chunksOf 2 . parseInputList

parseInputList :: String -> [Tree Integer]
parseInputList = map parseLine . filter (not.null) . lines

pair :: [x] -> (x,x)
pair [a,b] = (a,b)
pair _ = error "Couldn't find pair"

parseLine :: String -> Tree Integer
parseLine = fst . head . readP_to_S (tree <* eof)

tree :: ReadP (Tree Integer)
tree = do
  c <- peek 
  if isDigit c
    then Leaf     <$> num
    else Branches <$> (char '[' *> (tree `sepBy` char ',') <* char ']')

peek :: ReadP Char
peek = do
  remaining <- look
  case remaining of
    (h:_) -> pure h
    [] -> pfail

num :: (Read a, Num a) => ReadP a
num = read <$> digits
  where
  digit = satisfy isDigit
  digits = many1 digit

-- | Testing parseInput
-- >>> parseInput testInputSmall
-- [([[1],[2,3,4]],[[1],4])]
-- 
testInputSmall :: String
testInputSmall = drop 1 [r|
[[1],[2,3,4]]
[[1],4]
|]

testInput :: String
testInput = drop 1 [r|
[1,1,3,1,1]
[1,1,5,1,1]

[[1],[2,3,4]]
[[1],4]

[9]
[[8,7,6]]

[[4,4],4,4]
[[4,4],4,4,4]

[7,7,7,7]
[7,7,7]

[]
[3]

[[[]]]
[[]]

[1,[2,[3,[4,[5,6,7]]]],8,9]
[1,[2,[3,[4,[5,6,0]]]],8,9]
|]