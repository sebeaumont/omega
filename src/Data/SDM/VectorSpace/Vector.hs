{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}

module Data.SDM.VectorSpace.Vector
  ( Idx
  , Vector
  , Matrix
  , indexSet
  , indexList
  , toVector
  , toIndex
  , vectorIV
  , fromIV
  , fromInitList
  , withZeros
  -- * index twiddling
  , union
  , diff
  , disjoint
  , size
  ) where

import GHC.Generics
import Control.DeepSeq

import qualified Data.Set as Set
import Data.Set (Set)

-- | Types of Vectors with index sets whihc can represent
-- both sparse and dense vectors.

newtype Idx = Idx (Set Int) deriving (Show, Generic, NFData)

newtype Vector v = Vect (Set (Int, v)) deriving (Show)

newtype Matrix v = Mat (Set (Int, Int, v)) deriving (Show)


-- constructor helpers

toIndex :: [Int] -> Idx
toIndex !ix = Idx $ Set.fromList ix

indexSet :: Idx -> Set Int
indexSet (Idx !i) = i

indexList :: Idx -> [Int]
indexList (Idx !i) = Set.toList i 

toVector :: Ord b => [Int] -> [b] -> Vector b
toVector !i !v = Vect $ Set.fromList $ zip i v

vectorIV :: Vector v -> [(Int,v)]
vectorIV (Vect !iv) = Set.toList iv

fromIV :: (Ord v, Num v) => [(Int,v)] -> Vector v
fromIV ps = Vect $ Set.fromList ps

fromInitList :: (Ord v, Num v) => v -> [Int] -> Vector v
fromInitList !v !ix = toVector ix (repeat v)

withZeros :: (Ord v, Num v) => [Int] -> Vector v
withZeros = fromInitList 0

union :: Idx -> Idx -> Idx
union (Idx !a) (Idx !b) = Idx $ Set.union a b  

diff :: Idx -> Idx -> Idx
diff (Idx !a) (Idx !b) = Idx $ Set.difference a b  

disjoint :: Idx -> Idx -> Idx
disjoint (Idx !a) (Idx !b) =
  Idx $ Set.difference (Set.union a b) (Set.intersection a b) 

size :: Idx -> Int
size (Idx !a) = Set.size a
