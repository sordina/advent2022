{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Advent17 where

import Text.RawString.QQ (r)
import Control.Monad.State (execState, replicateM_, State, gets)
import Data.List.Split (splitOn)
import Control.Lens ( Field1(_1), Field2(_2), view, (<<%=), (%=), Field3(_3) )
import Control.Arrow (Arrow ((***), first, second))
import Data.Maybe (catMaybes)
import Debug.Trace (trace)

import qualified Data.Set as Set

type Coord = (Int,Int)
type Canvas = Set.Set Coord
type Pieces = [Canvas]
type Instructions = String
type World = (Canvas, Pieces, Instructions)

day17 :: String -> Int
day17 = day17N 2022

day17N :: Int -> String -> Int
day17N n (cleanInput -> s) = pred $ height $ {- traceWorld $ -} flip execState (mempty, cycle shapes, cycle s) $ replicateM_ n piece

cleanInput :: String -> String
cleanInput = filter (`elem` "<>")

traceWorld :: World -> World
traceWorld w@(c,_,_) = trace output w
  where
  output = unlines (reverse ls)
  ls = map line [0..h]
  line y = map (col y) [0..8]
  col y x
    | y < 1 && (x < 1 || x > 7) = '+'
    | y < 1 = '-'
    | x < 1 = '|'
    | x > 7 = '|'
    | Set.member (x,y) c = '#'
    | otherwise = '.'
  h =  height w

height :: World -> Int
height (c, _, _) =
  if Set.null c
    then 1
    else succ $ maximum $ Set.map snd c

{- |
The tall, vertical chamber is exactly seven units wide. Each rock appears so
that its left edge is two units away from the left wall and its bottom edge is
three units above the highest rock in the room (or the floor, if there isn't
one).
-}
piece :: State World ()
piece = do
  y    <- gets height
  rock <- head <$> (_2 <<%= drop 1)
  let placed = Set.map ((+3) *** (\h -> h+y+3)) rock
  fall placed

fall :: Canvas -> State World ()
fall rock = do
  rock' <- gust rock
  rock'' <- plummet rock'
  -- modify traceWorld -- Debugging
  if rock'' == rock'
    then _1 %= (<> rock'')
    else fall rock''

clips :: Canvas -> State World Bool
clips rock = do
  c <- gets (view _1)
  let
    s1 = Set.size rock
    s2 = Set.size c
    n  = rock <> c
    s3 = Set.size n
  pure (s3 < s1 + s2)

gust :: Canvas -> State World Canvas
gust rock = do
  i <- head <$> (_3 <<%= drop 1)
  let 
    rock' = case i of
      '<' -> Set.map (first pred) rock
      '>' -> Set.map (first succ) rock
      _   -> rock
    xs = Set.map fst rock'
    xMin = minimum xs
    xMax = maximum xs
  clip <- clips rock'
  if xMin < 1 || xMax > 7 || clip
    then pure rock
    else pure rock'

plummet :: Canvas -> State World Canvas
plummet rock = do
  let
    rock' = Set.map (second pred) rock
    ys = Set.map snd rock'
    yMin = minimum ys
  clip <- clips rock'
  if yMin < 1 || clip
    then pure rock
    else pure rock'

-- | Shapes broken into their corresponding canvases
-- >>> shapes
-- [fromList [(0,0),(1,0),(2,0),(3,0)],fromList [(0,1),(1,0),(1,1),(1,2),(2,1)],fromList [(0,0),(1,0),(2,0),(2,1),(2,2)],fromList [(0,0),(0,1),(0,2),(0,3)],fromList [(0,0),(0,1),(1,0),(1,1)]]

shapes :: [Canvas]
shapes = map shape $ splitOn [""] $ lines shapesString

shape :: [String] -> Canvas
shape = Set.fromList . concat . zipWith row [0..] . reverse

row :: Int -> String -> [Coord]
row y = catMaybes . zipWith col [0..]
  where
    col x '#' = Just (x,y)
    col _ _ = Nothing

shapesString :: String
shapesString = drop 1 [r|
####

.#.
###
.#.

..#
..#
###

#
#
#
#

##
##
|]

-- | Testing day17 on Sample Input
-- >>> day17N 2022 sampleInput
-- WAS 3069
-- NOW 3068

sampleInput :: String
sampleInput = drop 1 [r|
>>><<><>><<<>><>>><<<>>><<<><<<>><>><<>>
|]
