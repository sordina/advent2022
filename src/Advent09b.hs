{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE BlockArguments #-}

module Advent09b where

import Debug.Trace
import Data.Foldable (for_)
import Control.Lens
import Control.Arrow ((***))
import Control.Monad
import Control.Monad.State
import Text.RawString.QQ (r)
import qualified Data.Set as Set
import qualified Data.IntMap.Strict as IntMap

-- | Testing day9b
-- >>> day9b testInput
-- 36

testInput :: String
testInput = drop 1 [r|
R 5
U 8
L 8
D 3
R 17
D 10
L 25
U 20
|]

type Point = (Int,Int)
type World = (Set.Set Point, IntMap.IntMap Point)

day9b :: String -> Int
day9b s = Set.size $ {- traceWithId drawTails $ -} (\(x,_) -> x) $ execState (instructions s) (Set.empty, knots)

traceWithId :: (a -> String) -> a -> a
traceWithId f x = trace (f x) x

knots :: IntMap.IntMap Point
knots = IntMap.fromList (zip [0..9] (repeat (0,0)))

instructions :: String -> State World ()
instructions s = do
  mapM_ (move 0) $ parseInput s
  logT

move :: Int -> (Point, Int) -> State World ()
move n (d,m) = replicateM_ m $ do
  logT

  -- Move the head and get its new position
  _2 . at n . _Just %= add d

  -- Get the current positions of head and tail
  h_ <- gets $ view (_2 . at n)
  t_ <- gets $ view (_2 . at (succ n))

  for_ ((,) <$> h_ <*> t_) $ \(h,t) ->
    when (not (touching h t)) (moveTail (succ n))

logT :: State World ()
logT = do
  t_ <- gets $ view (_2 . at 9)
  for_ t_ $ \t -> _1 %= Set.insert t

moveTail :: Int -> State World ()
moveTail n = do
  h_ <- gets $ view (_2 . at (pred n))
  t_ <- gets $ view (_2 . at n)
  for_ ((,) <$> h_ <*> t_) $ \(h,t) -> do
    let d = (sign *** sign) $ diff h t
    move n (d,1)

sign :: Int -> Int
sign w
  | w > 0 = 1
  | w < 0 = -1
  | otherwise = 0

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
  item i = error i

drawTails :: Set.Set Point -> String
drawTails s = unlines $ map dy ry
  where
  dy y = map (dx y) rx
  dx y x = if Set.member (x,y) s then '#' else ' '
  es = Set.elems s
  rx = [minimum (map fst es) ..  maximum (map fst es)]
  ry = [minimum (map snd es) ..  maximum (map snd es)]

