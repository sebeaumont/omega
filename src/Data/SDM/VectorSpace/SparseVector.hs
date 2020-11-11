{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE Strict #-}

module Data.SDM.VectorSpace.SparseVector where

import GHC.Generics ( Generic )
import Control.DeepSeq ( NFData )
import Data.Set (Set)
import Data.SDM.VectorSpace.Vector 
    ( Matrix, Vector, Idx, toIndex, indexSet, toVector, union, diff )

-- | Sparse vectors.
newtype SparseBitVector = BVec Idx deriving (Show, Generic, NFData)

newtype SparseVector v = SVec (Vector v) 
  deriving (Show, Generic, NFData)

newtype SparseMatrix v = SMat (Matrix v) 
  deriving (Show, Generic, NFData)

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
