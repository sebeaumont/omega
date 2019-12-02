{-# LANGUAGE BangPatterns #-}
module Data.SDM.SemanticVector where

import Data.List
import Data.SDM.VectorSpace
import Data.SDM.Entropy

-- vector space dimensions and number of bits in sparse random vector
p :: Int
p = 16

d :: Int
d = 32768

-- | SemanticVector 
-- one bit vector for basis and one the the superposed result
data SemanticVector = SV { sK :: !SparseBitVector
                         , sV :: !DenseBitVector
                         } deriving (Show)

-- | Superpose SemanticVector u with sparse vector
super :: SemanticVector -> SparseBitVector -> SemanticVector
super !u !sv = u { sV = superpose (sV u) sv }

-- | NOTA superposition is associative (Monoid) => this can be fused
-- and we can batch superpose a frame...
-- if we dont mind reflection we can sum up all the sparse vectors
-- in a frame and then do only O(2n) vector adds

mutual :: [SemanticVector] -> [SemanticVector]
mutual !vs = [super u mv | u <- vs] where
  mv = let !zv = bitVecFromList []
       in foldl' add zv [sK v | v <- vs] 

-- | Make a new SemanticVector - requires entropy for random number generation.
makeSemanticVector :: MonadEntropy m => m SemanticVector
makeSemanticVector = do
  !svK' <- makeSparseRandomBitVector p d
  let !svV' = denseZeroBVector d -- bitVecFromList []
  return $ SV svK' svV'


