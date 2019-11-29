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

-- | Your mission Jim... since we have to index the `U.Vector` using
-- `Int` as this is given in its type unless we can convert other
-- Integral types to Int freely? then the type of `a` is inferred to
-- be Int and we lose the polymorphism even though there is no real
-- connection other than computing the index in the target `U.Vector`

idxFoo :: Integral b => SparseBitVector -> (Int, [(Int, b)])
idxFoo bv =
  let foo (i,b) = (i, fromIntegral $ 2^b)
      maxi = (Set.findMax $ index bv) `div` bitsPerWord
  in (maxi, map foo [wordBit i | i <- Set.toList $ index bv]) where

bitVecToDense :: (Integral a, U.Unbox a) => SparseBitVector -> U.Vector a
bitVecToDense bv =
  let (size, wib) = idxFoo bv
      vector = U.replicate (fromIntegral (size+1)) 0
  in vector U.// wib
