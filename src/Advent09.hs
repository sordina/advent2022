{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE BlockArguments #-}

module Advent09 where

import Control.Monad ( replicateM_, unless )
import Text.RawString.QQ (r)
import qualified Data.Set as Set
import Control.Monad.State ( State, gets, execState )
import Control.Lens
    ( view, (%=), Field1(_1), Field2(_2), Field3(_3) )

-- | Testing day9
-- >>> day9 testInput
-- 13

testInput :: String
testInput = drop 1 [r|
R 4
U 4
L 3
D 1
R 4
D 1
L 5
R 2
|]

type Point = (Int,Int)
type World = (Set.Set Point, Point, Point)

day9 :: String -> Int
day9 s = Set.size $ (\(x,_,_) -> x) $ execState (instructions s) (Set.empty, (0,0), (0,0))

instructions :: String -> State World ()
instructions = mapM_ move . parseInput

move :: (Point, Int) -> State World ()
move (d,m) = replicateM_ m $ do
  t <- gets (view _3)
  _1 %= Set.insert t
  _2 %= add d
  h <- gets (view _2)
  unless (touching h t) (moveTail h t)

moveTail :: Point -> Point -> State World ()
moveTail h t = do
  let
    d@(_,_) = diff h t
    d' = case d of
           ( 2,y) -> ( 1, y)
           (-2,y) -> (-1, y)
           (x, 2) -> (x, 1)
           (x,-2) -> (x,-1)
           z -> error (show z)
  _3 %= add d'
  t' <- gets (view _3)
  _1 %= Set.insert t'

diff :: Point -> Point -> Point
diff (a,b) (c,d) = (a-c,b-d)

add :: Point -> Point -> Point
add (a,b) (c,d) = (a+c,b+d)

touching :: Point -> Point -> Bool
touching (a,b) (c,d) = abs (a-c) <=1 && abs (b-d) <= 1

parseInput :: String -> [(Point, Int)]
parseInput = map item . lines
  where
  item ('U':' ':t) = ((0,-1), read t)
  item ('D':' ':t) = ((0, 1), read t)
  item ('L':' ':t) = ((-1,0), read t)
  item ('R':' ':t) = (( 1,0), read t)
  item i = error $ "Couldn't parse item: " <> i

