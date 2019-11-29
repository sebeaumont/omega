module Data.SDM.VectorSpace.SparseRandom where

import Control.Monad
import Data.SDM.Entropy
import Data.SDM.VectorSpace.Vector
import Data.SDM.VectorSpace.SparseVector


-- | Sparse distributed vectors have a uniform probability `p`/`d` of
-- an index at given dimensionality `d`. To construct sparse random
-- bit vector in the presence of entropy we supply `p` and `d`. It is
-- important to note that the upper bound on `d` and therefor `p` is
-- constrained bt the type of the index `a` and in this implementation
-- using finite `Set`s this is bounded by the max Int value. 

makeSparseRandomBitVector  :: (MonadEntropy m) => Int -> Int -> m SparseBitVector
makeSparseRandomBitVector p d = (BVec . toIndex) <$> (getRandomList p d)


-- | Make batch of `n` random index SparseBitVectors
-- withEntropy $ makeSparseRandomBitVectors 10 16 16496 :: IO ([SparseBitVector Word16])

makeSparseRandomBitVectors :: (MonadEntropy m) => Int -> Int -> Int -> m [SparseBitVector]
makeSparseRandomBitVectors n p d = replicateM n (makeSparseRandomBitVector p d)

-- | p d v=0 sparse random with values...
-- withEntropy $ makeSparseRandomVector 16 16392 :: IO (SparseVector Word16 Double)

makeSparseRandomVector :: (MonadEntropy m, Num v, Ord v) => Int -> Int -> m (SparseVector v)
makeSparseRandomVector p d = (SVec . toZeros) <$> (getRandomList p d)

-- | Could make this a vector of vectors... (matrix) rather than a list
makeSparseRandomVectors :: (MonadEntropy m, Num v, Ord v) => Int -> Int -> Int -> m [SparseVector v]
makeSparseRandomVectors n p d = replicateM n (makeSparseRandomVector p d)
