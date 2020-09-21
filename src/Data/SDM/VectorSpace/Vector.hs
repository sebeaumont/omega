{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}

module Data.SDM.VectorSpace.Vector where

import GHC.Generics
import Control.DeepSeq

import qualified Data.Set as Set
import Data.Set (Set)

-- | Types of Vectors with index sets

newtype Idx = Idx (Set Int) deriving (Show, Generic, NFData)

newtype Vector v = Vect (Set (Int,v)) deriving (Show)

newtype Matrix v = Mat (Set (Int,Int,v)) deriving (Show)


-- constructor helpers

toIndex :: [Int] -> Idx
toIndex !ix = Idx $ Set.fromList ix

--{-# INLINE indexSet #-}
indexSet :: Idx -> Set Int
indexSet (Idx !i) = i

indexToList :: Idx -> [Int]
indexToList (Idx !i) = Set.toList i 

toVector :: Ord b => [Int] -> [b] -> Vector b
toVector !i !v = Vect $ Set.fromList $ zip i v

vectorToList :: Vector v -> [(Int,v)]
vectorToList (Vect !iv) = Set.toList iv

toInitVector :: (Ord v, Num v) => v -> [Int] -> Vector v
toInitVector !v !ix = toVector ix (repeat v)

toZeros :: (Ord v, Num v) => [Int] -> Vector v
toZeros = toInitVector 0

--
--{-# INLINE union #-}
union :: Idx -> Idx -> Idx
union (Idx !a) (Idx !b) = Idx $ Set.union a b  

diff :: Idx -> Idx -> Idx
diff (Idx !a) (Idx !b) = Idx $ Set.difference a b  

disjointUnion :: Idx -> Idx -> Idx
disjointUnion (Idx !a) (Idx !b) =
  Idx $ Set.difference (Set.union a b) (Set.intersection a b) 

--{-# INLINE size #-}
size :: Idx -> Int
size (Idx !a) = Set.size a
