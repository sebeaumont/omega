module Data.SDM.VectorSpace.DenseVector where

import Data.Bits
import Data.Word
import Data.Set (Set)
import qualified Data.Set as Set
import qualified Data.Vector.Unboxed as U

import Data.SDM.VectorSpace.SparseVector

-- | Dense bit vectors based on unboxed vectors where we pack the bits into `Word`s
newtype DenseBitVector = DBVec (U.Vector Word)


{-# INLINE bitsPerWord #-}
bitsPerWord :: Integral a => a
bitsPerWord = round $ logBase 2 (fromIntegral $ (maxBound :: Word))


{-# INLINE wordBit #-}
wordBit :: Integral a => a -> (a, a)
wordBit n = divMod n bitsPerWord

{-# INLINE idxFoo #-}
-- this doesn't actually work yet as we may need to merge idx with (+) 
idxFoo :: Integral b => SparseBitVector -> (Int, [(Int, b)])
idxFoo bv =
  let foo (i,b) = (i, fromIntegral $ 2^b)
      maxi = (Set.findMax $ index bv) `div` bitsPerWord
      idx = [wordBit i | i <- Set.toList $ index bv]
  in (maxi, map foo idx) where

bitVecToDense :: (Integral a, U.Unbox a) => SparseBitVector -> U.Vector a
bitVecToDense bv =
  let (size, wib) = idxFoo bv
      vector = U.replicate (fromIntegral (size+1)) 0
  in vector U.// wib


class Densify a where
  toDense:: a -> U.Vector Word
  
instance Densify SparseBitVector where
  toDense = bitVecToDense
  
  
-- | Incremental union/merge of sorted lists of index pairs takes
-- binary function of values. N.B. due to non-greedy behaviour only
-- applies binary fn to `snd` of matched `fst` in either list so this
-- must be applied each time we wish to aggregate values.  So
-- for duplicated `fst` or indexes e.g. we would get:
--  `unionWith` (+) [(1,2),(1,3),(1,4)] [(1,2),(1,3),(1,4)] = [(1,4),(1,6),(1,8)]
--  rather than [(1,18)]

{-# INLINE unionWith #-}
unionWith :: (Ord a, Num b) => (b -> b -> b) -> [(a, b)] -> [(a, b)] -> [(a, b)]
unionWith f p1@((i1,v1):r1) p2@((i2,v2):r2)
  | i1 > i2 = (i2, v2) : unionWith f p1 r2
  | i1 < i2 = (i1, v1) : unionWith f r1 p2
  | i1 == i2 = (i1, f v1 v2) : unionWith f r1 r2 
unionWith _ l []  = l  
unionWith _ [] l = l
unionWith _ _ _ = []

