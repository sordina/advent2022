{-# LANGUAGE InstanceSigs #-}
module Utils where

import qualified Data.Map as Map
import qualified Data.Set as Set
import Debug.Trace
import Control.Monad (liftM2)
import Control.Arrow ((&&&))

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

traceWithId :: (a -> String) -> a -> a
traceWithId f x = trace (f x) x

drawSet :: Set.Set (Int,Int) -> String
drawSet s = show (xl,yl) <> "~" <> show (xh,yh) <> "\n" <> (unlines $ map line [yl..yh])
  where
  line y  = map (\x -> if Set.member (x,y) s then '#' else ' ')  [xl..xh]
  (xl,xh) = (Set.findMin &&& Set.findMax) $ Set.map fst s
  (yl,yh) = (Set.findMin &&& Set.findMax) $ Set.map snd s


data IntX a = Val a | Inf | NegInf
  deriving (Eq)

instance Show a => Show (IntX a) where
  show Inf = "Inf"
  show NegInf = "-Inf"
  show (Val x) = show x

instance Ord a => Ord (IntX a) where
  compare Inf Inf = EQ
  compare NegInf NegInf = EQ
  compare NegInf _ = LT
  compare Inf _ = GT
  compare _ Inf = LT
  compare _ NegInf = GT
  compare (Val x) (Val y) = x `compare` y

instance Enum a => Enum (IntX a) where
  pred Inf = Inf
  pred NegInf = NegInf
  pred (Val x) = Val (pred x)
  succ Inf = Inf
  succ NegInf = NegInf
  succ (Val x) = Val (succ x)
  toEnum x = Val (toEnum x)
  fromEnum Inf = error "Inf"
  fromEnum NegInf = error "NegInf"
  fromEnum (Val x) = fromEnum x

instance (Eq a, Num a) => Num (IntX a) where
  (+) :: Num a => IntX a -> IntX a -> IntX a
  Inf + NegInf = error "Inf + NegInf"
  NegInf + Inf = error "NegInf + Inf"
  NegInf + _ = NegInf
  _ + NegInf = NegInf
  Inf + _ = Inf
  _ + Inf = Inf
  (Val x) + (Val y) = Val (x + y)

  (*) :: Num a => IntX a -> IntX a -> IntX a
  Inf * y =
    case signumX y of
      LT -> NegInf
      GT -> Inf
      EQ -> error "* 0"
  NegInf * y =
    case signumX y of
      LT -> Inf
      GT -> NegInf
      EQ -> error "* 0"
  y * Inf =
    case signumX y of
      LT -> NegInf
      GT -> Inf
      EQ -> error "* 0"
  y * NegInf =
    case signumX y of
      LT -> Inf
      GT -> NegInf
      EQ -> error "* 0"
  (Val x) * (Val y) = Val (x*y)

  abs :: (Eq a, Num a) => IntX a -> IntX a
  abs NegInf = Inf
  abs Inf = Inf
  abs (Val x) = Val (abs x)

  signum :: Num a => IntX a -> IntX a
  signum x = case signumX x of
    LT -> -1
    GT -> 1
    EQ -> 0

  fromInteger x = Val (fromInteger x)

  negate Inf = NegInf
  negate NegInf = Inf
  negate (Val x) = Val (negate x)

signumX :: (Num a, Eq a) => IntX a -> Ordering
signumX Inf = GT
signumX NegInf = LT
signumX (Val x) =
  case signum x of
    -1 -> LT
    1 -> GT
    0 -> EQ
    _ -> error "Expecting signum result"
