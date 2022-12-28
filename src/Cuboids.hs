{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE InstanceSigs #-}

-- | Borrowed from Advent 2021 Day 22b: https://github.com/sordina/advent2021/blob/solutions/src/Advent22b.hs

module Cuboids where

-- $setup
-- >>> :set -XDataKinds
-- >>> :set -XTypeApplications

import Data.Set qualified as Set
import Data.Vector qualified as V
import Data.Foldable (toList)
import Data.Tuple (swap)
import Control.Arrow ((&&&))
import Data.Maybe (catMaybes)
import GHC.TypeNats (Nat, natVal, KnownNat)
import Data.Data (Proxy(..))
import Data.List (intercalate)

-- * Types

-- Elements are ranges in each dimension
type Cuboid (n :: Nat) = Vec n P2

type P2 = (Int,Int)

newtype Vec (n :: Nat) a = UnsafeMkVec { getVector :: V.Vector a }
    deriving (Eq, Ord, Functor, Foldable, Traversable)

instance Show a => Show (Vec n a) where
    show :: Show a => Vec n a -> String
    show v = "V" <> show (length v) <> " <" <> intercalate "," (toList (fmap show v)) <> ">"

-- * Constructors

zipWithMVec :: Monad m => (a -> b -> m c) -> Vec n a -> Vec n b -> m (Vec n c)
zipWithMVec f (UnsafeMkVec a) (UnsafeMkVec b) = UnsafeMkVec <$> V.zipWithM f a b

mapMVec :: Monad m => (a -> m b) -> Vec n a -> m (Vec n b)
mapMVec f (UnsafeMkVec a) = UnsafeMkVec <$> V.mapM f a

mkVec :: forall n a. KnownNat n => [a] -> Maybe (Vec n a)
mkVec l
    | n == length l = Just (UnsafeMkVec (V.fromListN n l))
    | otherwise = Nothing
    where
    n = fromIntegral (natVal (Proxy @n))

mkVec2 :: (a,a) -> Vec 2 a
mkVec2 (a,b) = UnsafeMkVec (V.fromListN 2 [a,b])

unVec2 :: Vec 2 a -> (a,a)
unVec2 v =
  case V.toList (getVector v) of
    [a,b] -> (a,b)
    _ -> error "impossible"

mkVec3 :: (a,a,a) -> Vec 3 a
mkVec3 (a,b,c) = UnsafeMkVec (V.fromListN 3 [a,b,c])


-- * Operations for working with cuboids
-- 
-- NOTE: Remember that coordinates represent 'voxels' here, not edges,
--       therefore, [(1,1),(1,1),(1,1)] has a volume of 1
--       and the intersection of [(1,1),(1,1),(1,1)] and [(1,1),(1,1),(1,1)] is [(1,1),(1,1),(1,1)]

-- | A -~ B Subtracts B from A
--   It seems that this should be able to be accomplished by just taking the left hand side
--   of a composite operation, however this yields the wrong value.
--   Investigation into why this is the case would be worth while.
-- 
(-~) :: Cuboid n -> Cuboid n -> Set.Set (Cuboid n)
a -~ b = case compositeCuboids a b of
    Nothing    -> Set.singleton a
    Just (s,t) -> s Set.\\ t

-- | Calculates the volume of a cuboid.
-- >>> :set -XDataKinds
-- >>> :set -XTypeApplications
-- >>> volume <$> mkVec @0 []
-- Just 1
-- >>> volume <$> mkVec @1 [(1,1)]
-- Just 1
-- >>> volume <$> mkVec @2 [(1,2), (3,4)]
-- Just 4
-- >>> volume <$> mkVec @3 [(1,2), (3,4), (5,6)]
-- Just 8
volume :: Cuboid n -> Int
volume rs = product $ fmap (succ . uncurry (-) . swap) rs

center :: Cuboid n -> Vec n Int
center = fmap (uncurry avg)

avg :: Integral a => a -> a -> a
avg a b = (a+b) `div` 2

-- | A possible intersection of two cuboids
intersectCuboids :: Cuboid n -> Cuboid n -> Maybe (Cuboid n)
intersectCuboids l r = mapM (\(_,c,_) -> Just c) =<< zipWithMVec compositeR l r

-- | The regions represented by sub-cuboids that belong to cube A and cube B
--   the intersecting region is part of both sets.
-- 
--   The function exists in this form so that it can be used for subsequent
--   addition or subtraction operations.
compositeCuboids :: Cuboid n -> Cuboid n -> Maybe (Set.Set (Cuboid n), Set.Set (Cuboid n))
compositeCuboids rs1 rs2 = (possible gl &&& possible gr) <$> zipWithMVec compositeR rs1 rs2
    where
    gl (l, c, _) = c:l
    gr (_, c, r) = c:r
    possible f = Set.fromList . toList . mapM f

-- Splits an intersection into mutually exclusive regions and intersection (a independent, intersecting, b independent)
-- 
-- >>> compositeR (0,1) (2,3) -- Independent
-- >>> compositeR (0,1) (0,1) -- Identical
-- >>> compositeR (0,1) (1,2) -- Partial overlap
-- >>> compositeR (1,2) (0,1) -- Partial overlap, other direction
-- >>> compositeR (1,2) (0,3) -- Subsumption
-- >>> compositeR (0,3) (1,2) -- Subsumption, other direction
-- Nothing
-- Just ([],(0,1),[])
-- Just ([(0,0)],(1,1),[(2,2)])
-- Just ([(2,2)],(1,1),[(0,0)])
-- Just ([],(1,2),[(0,0),(3,3)])
-- Just ([(0,0),(3,3)],(1,2),[])
-- 
compositeR :: P2 -> P2 -> Maybe ([P2], P2, [P2])
compositeR (a1,a2) (b1,b2)
    | a1 <= b1 && a2 >= b1 && a2 <= b2 = Just (s a1 (pred b1), (b1,a2), s (succ a2) b2) -- a overlapping b from the left
    | b1 <= a1 && b2 >= a1 && b2 <= a2 = Just (s (succ b2) a2, (a1,b2), s b1 (pred a1)) -- a overlapping b from the right
    | a1 >= b1 && a2 <= b2             = Just ([], (a1,a2), s b1 (pred a1) ++ s (succ a2) b2) -- a inside b
    | b1 >= a1 && b2 <= a2             = Just (s a1 (pred b1) ++ s (succ b2) a2, (b1,b2), []) -- b inside a
    | otherwise                        = Nothing -- independent
    where
    s x y = catMaybes [significant x y]

compositeR' :: P2 -> P2 -> ([P2], Maybe P2, [P2])
compositeR' (a1,a2) (b1,b2)
    | a1 <= b1 && a2 >= b1 && a2 <= b2 = (s a1 (pred b1), Just (b1,a2), s (succ a2) b2) -- a overlapping b from the left
    | b1 <= a1 && b2 >= a1 && b2 <= a2 = (s (succ b2) a2, Just (a1,b2), s b1 (pred a1)) -- a overlapping b from the right
    | a1 >= b1 && a2 <= b2             = ([], Just (a1,a2), s b1 (pred a1) ++ s (succ a2) b2) -- a inside b
    | b1 >= a1 && b2 <= a2             = (s a1 (pred b1) ++ s (succ b2) a2, Just (b1,b2), []) -- b inside a
    | otherwise                        = (s a1 a2, Nothing, s b1 b2) -- independent
    where
    s x y = catMaybes [significant x y]

-- Creates significant (non-zero) regions
significant :: Int -> Int -> Maybe (Int,Int)
significant a b
    | a <= b = Just (a,b)
    | otherwise = Nothing
