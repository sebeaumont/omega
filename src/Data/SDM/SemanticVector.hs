module Data.SDM.SemanticVector where

import Data.List
import Data.SDM.VectorSpace
import Data.SDM.Entropy

-- try sparse bits embedding
-- type Binary = Void -- XXX FIXME VectorSpace.SparseVector

p :: Int
p = 16

d :: Int
d = 32768


data SemanticVector = SV { sK :: SparseBitVector
                         , sV :: SparseBitVector
                         } deriving (Show)

{-
-- | distance
dist :: SemanticVector -> SemanticVector -> Double
dist u v = difference (sV u) (sV v)

-- | density
rho :: SemanticVector -> Double
rho u = density (sV u)
-}

-- | Superpose SemanticVector u with sparse vector
super :: SemanticVector -> SparseBitVector -> SemanticVector
super u sv = u { sV = add sv (sV u) }

-- | NOTA superposition is associative (Monoid) => this can be fused
-- and we can batch superpose a frame...
-- if we dont mind reflection we can sum up all the sparse vectors
-- in a frame and then do only O(2n) vector adds

mutual :: [SemanticVector] -> [SemanticVector]
mutual vs = [super u mv | u <- vs] where
  mv = let zv = bitVecFromList []
       in
         foldl' add zv [sK v | v <- vs] 

-- | Make a new SemanticVector - requires entropy for random number generation.
makeSemanticVector :: MonadEntropy m => m SemanticVector
makeSemanticVector = do
  svK' <- makeSparseRandomBitVector p d
  let svV' = bitVecFromList []
  return $ SV svK' svV'
