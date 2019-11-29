{-# LANGUAGE FlexibleInstances #-}
module Data.SDM.VectorSpace.DenseVector where

import Data.Bits
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
idxFoo :: (Bits b, Integral b) => SparseBitVector -> (Int, [(Int, b)])
idxFoo bv =
  let foo (i,b) = (i, fromIntegral $ 2^b)
      maxi = (Set.findMax $ index bv) `div` bitsPerWord
      idx = [wordBit i | i <- Set.toList $ index bv]
  in (maxi, mergeWith (.&.) (map foo idx)) where

    
bitVecToDense :: (Bits a, Integral a, U.Unbox a) => SparseBitVector -> U.Vector a
bitVecToDense bv =
  let (size, wib) = idxFoo bv
      vector = U.replicate (fromIntegral (size+1)) 0
  in vector U.// wib

  
{-# INLINE mergeWith #-}
mergeWith :: Ord a => (b -> b -> b) -> [(a, b)] -> [(a, b)]
mergeWith f ((i1,v1):(i2,v2):rs) 
  | i1 == i2 = let m = (i1, f v1 v2) in mergeWith f (m:rs)
  | otherwise = (i1,v1) : mergeWith f ((i2,v2):rs)
mergeWith _ l@((_,_):[]) = l  
mergeWith _ []  = []  


class Densify a where
  toDense:: a -> U.Vector Word
  
instance Densify SparseBitVector where
  toDense = bitVecToDense
