{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE FlexibleInstances #-}
module Data.SDM.VectorSpace.DenseVector where

import Control.Monad.Primitive
import Data.Bit
import Data.Bits
import qualified Data.Set as Set
import qualified Data.Vector.Unboxed as U
import Data.SDM.VectorSpace.SparseVector

-- | Dense bit vectors based on unboxed vectors where we pack the bits into `Word`s
newtype DenseBitVector = DBVec (U.Vector Bit)

instance Show DenseBitVector where
  show (DBVec v) = "DBVec " ++ (show $ countBits v)

-- | Modify in place semantics for BitVectors
newtype DenseMBitVector m = DMBVec (U.MVector (PrimState m) Bit)


--{-# INLINE bitsPerWord #-}
bitsPerWord :: Integral a => a
bitsPerWord = round $ logBase 2 (fromIntegral (maxBound :: Word) :: Double)

-- {-# INLINE exp2 -#}
exp2 :: Int -> Word
exp2 !n = shiftL 1 n

--{-# INLINE wordBit #-}
wordBit :: Int -> (Int, Word)
wordBit !n = let (!i,!j) = divMod n bitsPerWord in (i, exp2 j) 

--{-# INLINE bitIndexes #-}
bitIndexes :: SparseBitVector -> (Int, [(Int, Word)])
bitIndexes !bv =
  let !maxi = Set.findMax (index bv) `div` bitsPerWord
      !idx = [wordBit i | i <- Set.elems $ index bv]
  in (maxi, mergeWith (.|.) idx)

--{-# INLiNE bitVecToDense #-}    
bitVecToDense ::  SparseBitVector -> U.Vector Word
bitVecToDense !bv =
  let (!size, !wib) = bitIndexes bv
      !vector = U.replicate (fromIntegral (size+1)) 0
  in vector U.// wib

--{-# INLINE mergeWith #-}
mergeWith :: Ord a => (b -> b -> b) -> [(a, b)] -> [(a, b)]
mergeWith f ((i1,v1):(i2,v2):rs) 
  | i1 == i2 = let m = (i1, f v1 v2) in mergeWith f (m:rs)
  | otherwise = (i1,v1) : mergeWith f ((i2,v2):rs)
mergeWith _ l@[(_,_)] = l  
mergeWith _ []  = []  

--{-# INLINE denseZeroBVector #-}
denseZeroBVector :: Int -> DenseBitVector
denseZeroBVector d = DBVec $ U.replicate d 0

-- | we dont go all the way to `DenseBitVector` here in case we want to
-- try different representations based on `Data.Vector.Unboxed` `Word`
-- XXX or is this totally wrong way around? This isn't adding any abstraction.

class Densify a where
  toDense:: a -> U.Vector Word
  
instance Densify SparseBitVector where
  {-# INLINE toDense #-}
  toDense = bitVecToDense

-- metric
distance :: DenseBitVector -> DenseBitVector -> Int  
distance (DBVec !u) (DBVec !v) = countBits $ zipBits xor u v

-- algebra
andv :: DenseBitVector -> DenseBitVector -> DenseBitVector
andv (DBVec !u) (DBVec !v) = DBVec $ zipBits (.&.) u v 

orv :: DenseBitVector -> DenseBitVector -> DenseBitVector
orv (DBVec !u) (DBVec !v) = DBVec $ zipBits (.|.) u v 

xorv :: DenseBitVector -> DenseBitVector -> DenseBitVector
xorv (DBVec !u) (DBVec !v) = DBVec $ zipBits xor u v 

negv :: DenseBitVector -> DenseBitVector
negv (DBVec !u) = DBVec $ invertBits u

zerov :: Int -> DenseBitVector
zerov = denseZeroBVector

-- TODO contraction/expansion of bit runs...
scalev :: Double -> DenseBitVector -> DenseBitVector
scalev _ !v = v


--{-# INLINE superpose #-}
superpose :: DenseBitVector -> SparseBitVector -> DenseBitVector
superpose (DBVec !dv) !sv =
  let !sv' = castFromWords $ toDense sv
  in DBVec $ zipBits (.|.) dv sv'

-- | this would be the best speed up for indexing

superposeM :: PrimMonad m => SparseBitVector -> DenseMBitVector m  -> m ()
superposeM !sv (DMBVec !dv) =
  let sv' = castFromWords $ toDense sv
  in zipInPlace (.|.) sv' dv


  
      
