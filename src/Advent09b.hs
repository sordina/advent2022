{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE NegativeLiterals #-}

module Advent09b where

import Utils (traceWithId)
import Data.Maybe
import Linear.V2
import Data.Foldable (for_)
import Control.Lens
import Control.Monad
import Control.Monad.State
import Text.RawString.QQ (r)
import qualified Data.Set as Set
import qualified Data.Sequence as Seq

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

type Point = V2 Int
type World = (Set.Set Point, Seq.Seq Point)

day9b :: String -> Int
day9b s = Set.size $ {- traceWithId drawTails $ -} fst $ execState (instructions s) (Set.empty, knots)

knots :: Seq.Seq Point
knots = Seq.fromList (replicate 10 0)

instructions :: String -> State World ()
instructions s = do
  mapM_ (move 0) $ parseInput s
  logT

logT :: State World ()
logT = do
  t_ <- gets $ preview (_2 . ix 9)
  for_ t_ $ \t ->_1 %= Set.insert t

-- Bad, Good!
getsJ :: MonadState s f => (s -> Maybe b) -> f b
getsJ f = fromMaybe (error "Got Nothing!") <$> gets f

move :: Int -> (Point, Int) -> State World ()
move n (d,m) = replicateM_ m $ do
  logT

  -- Move the head and get its new position
  _2 . ix n %= (+d)

  -- Get the current positions of head and tail
  h <- getsJ $ preview (_2 . ix n)
  t_ <- gets $ preview (_2 . ix (succ n))

  for_ t_ $ \t -> when (not (touching h t)) (moveTail (succ n))

moveTail :: Int -> State World ()
moveTail n = do
  h <- getsJ $ preview (_2 . ix (pred n))
  t <- getsJ $ preview (_2 . ix n)
  let d = signum $ h-t
  move n (d,1)

touching :: Point -> Point -> Bool
touching (V2 a b) (V2 c d) = abs (a-c) <=1 && abs (b-d) <= 1

parseInput :: String -> [(Point, Int)]
parseInput = map item . lines
  where
  item ('U':' ':t) = (V2  0 -1, read t)
  item ('D':' ':t) = (V2  0  1, read t)
  item ('L':' ':t) = (V2 -1  0, read t)
  item ('R':' ':t) = (V2  1  0, read t)
  item i = error i

drawTails :: Set.Set Point -> String
drawTails s = unlines $ map dy ry
  where
  dy y = map (dx y) rx
  dx y x = if Set.member (V2 x y) s then '#' else ' '
  rx = [Set.findMin (Set.map (view _x) s) ..  Set.findMax (Set.map (view _x) s)]
  ry = [Set.findMin (Set.map (view _y) s) ..  Set.findMax (Set.map (view _y) s)]

