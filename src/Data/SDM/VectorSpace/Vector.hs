{-# LANGUAGE BangPatterns #-}
module Data.SDM.VectorSpace.Vector where

import qualified Data.Set as Set
import Data.Set (Set)

-- | Types of Vectors with index sets

newtype Index = Idx (Set Int) deriving (Show)

newtype Vector v = Vect (Set (Int,v)) deriving (Show)

newtype Matrix v = Mat (Set (Int,Int,v)) deriving (Show)


-- constructor helpers

toIndex :: [Int] -> Index
toIndex !ix = Idx $ Set.fromList ix

indexSet :: Index -> Set Int
indexSet (Idx !i) = i

indexToList :: Index -> [Int]
indexToList (Idx !i) = Set.toList i 

toVector :: Ord b => [Int] -> [b] -> Vector b
toVector i v = Vect $ Set.fromList $ zip i v

vectorToList :: Vector v -> [(Int,v)]
vectorToList (Vect !iv) = Set.toList iv

toInitVector :: (Ord v, Num v) => v -> [Int] -> Vector v
toInitVector !v !ix = toVector ix (repeat v)

toZeros :: (Ord v, Num v) => [Int] -> Vector v
toZeros = toInitVector 0

--
union :: Index -> Index -> Index
union (Idx !a) (Idx !b) = Idx $ Set.union a b  

diff :: Index -> Index -> Index
diff (Idx !a) (Idx !b) = Idx $ Set.difference a b  

disjointUnion :: Index -> Index -> Index
disjointUnion (Idx !a) (Idx !b) =
  Idx $ Set.difference (Set.union a b) (Set.intersection a b) 

size :: Index -> Int
size (Idx !a) = Set.size a
