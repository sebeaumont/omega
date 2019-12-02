{-# LANGUAGE BangPatterns #-}
module Data.SDM.VectorSpace.SparseVector where

import Data.Set (Set)
import Data.SDM.VectorSpace.Vector

-- | Sparse vectors.
newtype SparseBitVector = BVec Index deriving (Show)

newtype SparseVector v = SVec (Vector v) deriving (Show)

newtype SparseMatrix v = SMat (Matrix v) deriving (Show)


-- | Construct non-random sparse vector from lists of indexes and values
--{-# INLINE fromList #-}
fromList :: Ord v => [Int] -> [v] -> SparseVector v
fromList is vs = SVec $ toVector is vs

-- | Construct binary (index only) sparse vector from lists of indexes
--{-# INLINE bitVecFromList #-}
bitVecFromList :: [Int]  -> SparseBitVector
bitVecFromList is = BVec $ toIndex is

--{-# INLINE index #-}
index :: SparseBitVector -> Set Int
index (BVec !i) = indexSet i

-- only bit vectors at present
--{-# INLINE add #-}
add :: SparseBitVector -> SparseBitVector -> SparseBitVector
add (BVec !u) (BVec !v) = BVec $ union u v

--{-# INLINE sub #-}
sub :: SparseBitVector -> SparseBitVector -> SparseBitVector
sub (BVec !u) (BVec !v) = BVec $ diff u v

--{-# INLINE distance #-}
distance :: SparseBitVector -> SparseBitVector -> Int
distance (BVec !u) (BVec !v) = size $ disjointUnion u v
