{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}

module Data.SDM.SemanticVector where

import GHC.Generics
import Control.DeepSeq
--import Data.Serialize

import Data.List
import Data.SDM.VectorSpace
import Data.SDM.Entropy

-- vector space dimensions and number of bits in sparse random vector

-- TODO add these to our Reader monad and start using it
-- perhaps stacked on MonadEntropy -- the p value could live in there
-- (as a [0,1] real) and D sit in the space context. We also need to
-- play with contraction and non Nat dimensionality...

p :: Int
p = 16

d :: Int
d = 32768

-- TODO derive: Generic, Serialize, NFData for this and products
-- | SemanticVector 
-- one bit vector for basis and one the the superposed result
data SemanticVector = SV { sK :: !DenseBitVector
                         , sV :: !DenseBitVector
                         } deriving (Show, Generic, NFData)

-- | Superpose SemanticVector with basis
super :: SemanticVector -> DenseBitVector -> SemanticVector
super !u !dv = u { sV = superpose (sV u) dv }

-- | NOTA superposition is associative (Monoid) => this can be fused
-- and we can batch superpose a frame...
-- if we dont mind reflection we can sum up all the sparse vectors
-- in a frame and then do only O(2n) vector adds

mutual :: [SemanticVector] -> [SemanticVector]
mutual !vs = [super u mv | u <- vs] where
  mv = foldl' orv (zerov d) [sK v | !v <- vs] 

{-
-- | Make a new SemanticVector - requires entropy for random number generation.
makeSemanticVector :: MonadEntropy m => m SemanticVector
makeSemanticVector = do
  !svK' <- makeSparseRandomBitVector p d
  let !svV' = denseZeroBVector d -- bitVecFromList []
  return $ SV svK' svV'
-}

-- | Make a new SemanticVector - requires entropy for random number generation.
makeSemanticVector :: MonadEntropy m => m SemanticVector
makeSemanticVector = do
  !svK' <- toDenseBitVector <$> makeSparseRandomBitVector p d
  let !svV' = denseZeroBVector d -- bitVecFromList []
  return $ SV svK' svV'


