{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE NegativeLiterals #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedLabels #-}

module Advent11 where

import Data.Char
import Data.List (sort)
import GHC.Generics
import Data.Generics.Labels ()
import Data.Foldable (toList)
import Text.RawString.QQ (r)
import Control.Monad
import Control.Monad.State
import Control.Lens
import Control.Applicative hiding (many)
import Text.ParserCombinators.ReadP hiding (optional, get)
import qualified Data.Sequence as Seq

-- | Testing day11
-- >>> day11 testInput

day11 :: String -> Integer
day11 s = product $ take 2 $ reverse $ sort $ map fst $ toList result
  where
  result = execState eval (fmap (0,) parsed)
  parsed = parseInput s

day11b :: String -> Integer
day11b = error "TODOb"

-- * Evaluation

type World = Seq.Seq (Integer, Monkey)

eval :: State World ()
eval = replicateM_ 20 evalRound

evalRound :: State World ()
evalRound = do
  ids <- gets (toList . fmap (who . snd))
  mapM_ evalMonkey ids

evalMonkey :: Int -> State World ()
evalMonkey x = do
  evalWhile (pop x) $ \i -> do
    ix x . _1 %= succ
    n <- evalExp x i
    let n' = n `div` 3
    t <- gets (preview (ix x . _2 . #next))
    case t of
      Nothing -> error "Couldn't find associated next"
      Just (q,a,b)
        | n' `mod` q == 0 -> evalThrowTo a n'
        | otherwise       -> evalThrowTo b n'

evalWhile :: State s (Maybe a) -> (a -> State s b) -> State s ()
evalWhile p f = do
  m <- p
  case m of
    Nothing -> pure ()
    Just x -> f x >> evalWhile p f

pop :: Int -> State World (Maybe Integer)
pop x = do
  v <- ix x . _2 . #items <<%= seqTail
  pure (v ^? ix 0)

evalExp :: Int -> Integer -> State World Integer
evalExp x i = do
  m <- gets (preview (ix x . _2 . #op))
  case m of
    Nothing -> error $ "Couldn't find expression for monkey " <> show x
    Just e -> pure (evalExp' i e)

evalThrowTo :: Int -> Integer -> State World ()
evalThrowTo x n = ix x . _2 . #items %= (Seq.|> n)

evalExp' :: Integer -> Exp -> Integer
evalExp' i = \case
  Old -> i
  Scalar s -> s
  Add a b -> evalExp' i a + evalExp' i b
  Multiply a b -> evalExp' i a * evalExp' i b

seqTail :: Seq.Seq a -> Seq.Seq a
seqTail s =
  case Seq.viewl s of
    Seq.EmptyL -> Seq.empty
    (_ Seq.:< xs) -> xs

-- * Parsing

parseInput :: String -> Seq.Seq Monkey
parseInput = fromParse text

fromParse :: ReadP a -> String -> a
fromParse p = fst . head . readP_to_S p

text :: Parser (Seq.Seq Monkey)
text = do
  monkies <- (monkey `sepBy1` (nl >> nl)) <* (many nl *> eof)
  pure (Seq.fromList monkies)

monkey :: Parser Monkey
monkey = do
  n     <- string "Monkey "                        *> num  <* string ":" <* nl
  is    <- string "  Starting items: "             *> itemsParser        <* nl
  oper  <- string "  Operation: new = "            *> operation          <* nl
  test1 <- string "  Test: divisible by "          *> num                <* nl
  test2 <- string "    If true: throw to monkey "  *> num                <* nl
  test3 <- string "    If false: throw to monkey " *> num
  pure $ Monkey n is oper (test1,test2,test3)

-- | testing itemsParser
-- >> (parseTest (itemsParser <* eof)) "123, 234, 1, 234"
-- fromList [123,234,1,234]

itemsParser :: Parser (Seq.Seq Integer)
itemsParser = Seq.fromList <$> (num `sepBy1` string ", ")

nl :: Parser String
nl = string "\n"

num :: (Read a, Num a) => Parser a
num = read <$> digits
  where
  digit = satisfy isDigit
  digits = many1 digit

-- | testing operation
-- >>> (fromParse (operation <* eof)) "old * old"
-- Multiply Old Old

peek :: Parser Char
peek = head <$> look

operation :: Parser Exp
operation =
  do
    h <- (Old <$ string "old") <|> (Scalar <$> num)
    t <- optional $ do
      spaces
      x <- char '+' <|> char '*'
      spaces
      e <- operation
      pure (x,e)
    case t of
      Nothing -> pure h
      Just ('+', e) -> pure (Add h e)
      Just ('*', e) -> pure (Multiply h e)
      Just s -> error $ "Invalid operation symbol: " <> show s

spaces :: Parser ()
spaces = void $ many $ char ' '

-- * Data Types

data Monkey = Monkey
  { who :: Int
  , items :: Seq.Seq Integer
  , op :: Exp
  , next :: (Integer, Int, Int)
  }
  deriving (Show, Generic)

data Exp
  = Add Exp Exp
  | Multiply Exp Exp
  | Scalar Integer
  | Old
  deriving (Show)

type Parser a = ReadP a -- Parsec () String a

-- * Test Data

-- | Testing parseInput
-- >>> parseInput testInputSingle
-- fromList [Monkey {who = 0, items = fromList [79,98], op = Multiply Old (Scalar 19), next = (23,2,3)}]
--
-- | Testing parseInput larger
-- >>> mapM_ print $ parseInput testInput
-- Monkey {who = 0, items = fromList [79,98], op = Multiply Old (Scalar 19), next = (23,2,3)}
-- Monkey {who = 1, items = fromList [54,65,75,74], op = Add Old (Scalar 6), next = (19,2,0)}
-- Monkey {who = 2, items = fromList [79,60,97], op = Multiply Old Old, next = (13,1,3)}
-- Monkey {who = 3, items = fromList [74], op = Add Old (Scalar 3), next = (17,0,1)}

testInputSingle :: String
testInputSingle = drop 1 [r|
Monkey 0:
  Starting items: 79, 98
  Operation: new = old * 19
  Test: divisible by 23
    If true: throw to monkey 2
    If false: throw to monkey 3
|]

testInput :: String
testInput = drop 1 [r|
Monkey 0:
  Starting items: 79, 98
  Operation: new = old * 19
  Test: divisible by 23
    If true: throw to monkey 2
    If false: throw to monkey 3

Monkey 1:
  Starting items: 54, 65, 75, 74
  Operation: new = old + 6
  Test: divisible by 19
    If true: throw to monkey 2
    If false: throw to monkey 0

Monkey 2:
  Starting items: 79, 60, 97
  Operation: new = old * old
  Test: divisible by 13
    If true: throw to monkey 1
    If false: throw to monkey 3

Monkey 3:
  Starting items: 74
  Operation: new = old + 3
  Test: divisible by 17
    If true: throw to monkey 0
    If false: throw to monkey 1
|]

