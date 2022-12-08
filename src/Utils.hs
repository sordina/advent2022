{-# LANGUAGE TypeApplications #-}

module Utils where

import qualified Data.Map as Map
import Control.Monad (liftM2)

newtype ShowString = Show String

instance Show ShowString where show (Show s) = s

fixEq :: Eq t => (t -> t) -> t -> t
fixEq f x
  | x == y = x
  | otherwise = fixEq f y
  where
  y = f x

parseGrid :: String -> Map.Map (Int,Int) Int
parseGrid = parseGrid' (read . pure)

parseGrid' :: (Char -> b) -> String -> Map.Map (Int,Int) b
parseGrid' f s = Map.fromList cs
  where
  ls = lines s
  cs = concat $ zipWith (\ y l -> zipWith (\ x c -> ((x, y), f c)) [0 .. ] l) [0..] ls

-- >>> crossProductZero (2,3)
-- [(0,0),(0,1),(0,2),(0,3),(1,0),(1,1),(1,2),(1,3),(2,0),(2,1),(2,2),(2,3)]

crossProductZero :: (Num a, Num b, Enum a, Enum b) => (a, b) -> [(a, b)]
crossProductZero (a,b) = liftM2 (,) [0..a] [0..b]
