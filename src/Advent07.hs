{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE QuasiQuotes #-}

module Advent07 where

import Debug.Trace
import Data.Char
import Data.Either
import Data.Bifunctor
import Data.List (foldl')
import qualified Data.Map as Map
import qualified Data.Set as Set
import Control.Monad
import Control.Applicative hiding (many)
import Text.ParserCombinators.ReadP
import Text.RawString.QQ (r)

type Command = Either String ()
type Output = Either String File
type Line = Either Command Output
type File = (Int,String)
type Path = [String]
type Tree = Map.Map Path (Set.Set File)
type State = (Path, Trie)

newtype Trie = Nodes { unNodes :: Map.Map String (Either File Trie) } deriving (Eq)

instance Show Trie where show t = unlines $ drawTrie "/" 2 t

day7 :: String -> Int
day7 = sum . filter (<= 100000) . map countTrie . subTries . traceShowId . buildTree . map parse . lines

-- * Tests

-- | Testing against small input for day7
-- >>> day7 testInput

testInput = drop 1 [r|
$ cd /
$ ls
dir a
14848514 b.txt
8504156 c.dat
dir d
$ cd a
$ ls
dir e
29116 f
2557 g
62596 h.lst
$ cd e
$ ls
584 i
$ cd ..
$ cd ..
$ cd d
$ ls
4060174 j
8033020 d.log
5626152 d.ext
7214296 k
|]

-- * Helpers

buildTree :: [Line] -> Trie
buildTree = snd . foldl' process ([], emptyTrie)

insertTrie :: Path -> File -> Trie -> Trie
insertTrie [] f@(s,n) (Nodes t) = Nodes $ Map.insert n (Left f) t
insertTrie (x:xs) f (Nodes t) =
  case Map.lookup x t of
    Nothing -> Nodes $ Map.insert x (Right $ insertTrie xs f emptyTrie) t
    Just (Right t') -> Nodes $ Map.insert x (Right $ insertTrie xs f t') t
    Just (Left _) -> error "wtf"

emptyTrie :: Trie
emptyTrie = Nodes Map.empty

countTrie :: Trie -> Int
countTrie (Nodes t) = sum (map fst ls) + sum (map countTrie rs)
  where
  ls = lefts es 
  rs = rights es 
  es = Map.elems t

subTries :: Trie -> [Trie]
subTries p@(Nodes t) = p : (subTries =<< rs)
  where
  rs = rights es 
  es = Map.elems t

withKeys :: Bifunctor f => Map.Map k (f v a) -> [f (k,v) (k,a)]
withKeys m = (\(k,v) -> bimap (k,) (k,) v) <$> Map.toList m

drawTrie :: String -> Int -> Trie -> [String]
drawTrie p n t@(Nodes t') = [ "- " <> p <> " (dir) [" <> show (countTrie t) <> "]"] <> files <> subDirs
  where
    files = map ((\x -> sp <> "- " <> x) . showFile) (lefts es)
    subDirs = (\(k,v) -> map (sp <>) (drawTrie k n v)) =<< rights (withKeys t')
    es = Map.elems t'
    sp = replicate n ' '
    showFile (a,b) = b <> " (file, size=" <> show a <> ")"

process :: State -> Line -> State
process (l,m) = \case
  Left (Right ()) -> (l,m) -- ls: ignore
  Left (Left dir) -> -- cd
    case dir of
      "/" -> ([], m)
      ".." -> (tail l, m)
      d -> (d:l, m)
  Right (Left d) -> (l,m) -- directory: ignore
  Right (Right f) -> (l, insertTrie (reverse l) f m)

-- * Parsers

line :: ReadP Line
line = do
  s <- take 1 <$> look
  if s == "$"
     then Left <$> command
     else Right <$> output

output :: ReadP Output
output = fmap Left dir <|> fmap Right file

digit  = satisfy isDigit
digits = many1 digit
dir = string "dir " *> look
file = do
  ds <- read <$> digits
  char ' '
  (ds,) <$> look

command :: ReadP Command
command = do
  void $ string "$ "
  fmap Left cd <|> (Right () <$ ls)

cd :: ReadP String
cd = do 
  void $ string "cd "
  look

ls :: ReadP ()
ls = void $ string "ls"

parse :: String -> Either Command Output
parse = fst . head . readP_to_S line


day7b :: String -> Int
day7b = error "TODO"

