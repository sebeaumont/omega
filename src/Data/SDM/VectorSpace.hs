{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
--{-# LANGUAGE RankNTypes #-}
--{-# LANGUAGE InstanceSigs #-}

module Data.SDM.VectorSpace ( module Data.SDM.VectorSpace.Vector
                            , module Data.SDM.VectorSpace.SparseVector
                            , module Data.SDM.VectorSpace.SparseRandom
                            , module Data.SDM.VectorSpace.DenseVector
                            ) where
import Control.Monad.Reader
import Control.Monad.Identity
import Data.SDM.VectorSpace.Vector
import Data.SDM.VectorSpace.SparseVector
import Data.SDM.VectorSpace.SparseRandom
import Data.SDM.VectorSpace.DenseVector

-- | A dimensional context within which to interpret vectors we use a
-- Double as this is most general! could make this a type param of Dimensions

data Dimensions = Dimensions { spaceDims :: !Double }

newtype Space a = Space (ReaderT Dimensions Identity a)
  deriving (Functor, Applicative, Monad, MonadReader Dimensions)

withDims :: Double -> Space a -> Identity a  
withDims n (Space s) = runReaderT s (Dimensions n)
  
class (Monad m) => MonadSpace m where
  liftSpace :: Space a -> m a

instance MonadSpace Space where
  liftSpace = id

dimensions :: MonadSpace m => m Double
dimensions = liftSpace $ Space $ asks spaceDims >>= \n -> return n


-- | VectorSpace, InnerProductSpace etc. over a Field `f` not sure if this is useful yet!
-- maybe a GADT to evaluate Vector algebra might be more useful...

{-
class VectorSpace v where
  add :: Vector v -> Vector v -> Vector v
  scale :: Vector v -> v -> Vector v
-}  
  
{-
{-# INLINE unionWith #-}
unionWith :: (Ord a, Num b) => (b -> b -> b) -> [(a, b)] -> [(a, b)] -> [(a, b)]
unionWith f p1@((i1,v1):r1) p2@((i2,v2):r2)
  | i1 > i2 = (i2, v2) : unionWith f p1 r2
  | i1 < i2 = (i1, v1) : unionWith f r1 p2
  | i1 == i2 = (i1, f v1 v2) : unionWith f r1 r2 
unionWith _ l []  = l  
unionWith _ [] l = l
unionWith _ _ _ = []

-- | Try and do merge f with sets
-- XXX TODO: could partition union of s1 s2 based on a and then reduce with f
{-# INLINE mergeWith #-}
mergeWith :: (Ord a, Ord b, Num b) => (b -> b -> b) -> Set(a,b) -> Set(a,b) -> Set(a,b)
mergeWith f s1 s2 =
  let l1 = Set.toAscList s1
      l2 = Set.toAscList s2
  in Set.fromList $ unionWith f l1 l2
-}  

-- | Add two sparse vectors.
--
-- we are inclined to ignore the dimensionality of one or other
-- arguments here on the assumption that it makes no sense for them to
-- be different. I'm thinking that the dimensionality of a sparse
-- vector is really in the interval greatest lower bound on the number of
-- indexes existing and the capacity of the index type and could be
-- part of the evaluation semantics rather than fixed in the
-- type... but we could take the max of course!
{-
add :: (Ord i, Ord v, Num v) => 
       SparseVector i v -> SparseVector i v -> SparseVector i v
-- Binary vectors
add (BVec !ud !ui) (BVec _ !vi) = BVec ud (Set.union ui vi)
-- Sparse typed value vectors
add (SVec !ud !ui) (SVec _ !vi) = SVec ud (mergeWith (+) ui vi)

-- mixed arithmetic useful?
add (SVec !ud !ui) (BVec _ !vi) =
  let zs = toZeros $ Set.toAscList vi
  in SVec ud (mergeWith (+) ui zs)
    
add (BVec _ !vi)  (SVec !ud !ui) =
  let zs = toZeros $ Set.toAscList vi
  in SVec ud (mergeWith (+) ui zs)

-- negate a vector

negatev :: (Ord i, Ord v, Num v) => SparseVector i v -> SparseVector i v
negatev (SVec d vs) = SVec d $ Set.map (\(i,v) -> (i, negate v)) vs
-- what does it mean to negate a bit vector?
negatev u@(BVec _ _) = u


-- | Set difference remove members of second list from first
-- so look into Data.Set instead of SortedList
difference :: Ord a => [a] -> [a] -> [a]
difference l1 [] = l1
difference [] _ = []
difference l1@(x:xs) l2@(y:ys)
  | x == y = difference xs ys
  | x < y = x : difference xs l2
  | x > y = difference l1 ys
difference _ _ = []


-- | Subtract

sub :: (Ord i, Ord v, Num v) =>
       SparseVector i v -> SparseVector i v -> SparseVector i v
-- Binary vectors \\ difference
sub (BVec !ud !ui) (BVec _ !vi) = BVec ud (Set.difference ui vi)
  
-- Sparse typed value vectors
sub u@(SVec _ _) v@(SVec _ _) = add u (negatev v)

-- TODO mixed arithmetic in so far as it makes sense...
sub u@(SVec _ _) (BVec _ _) = u
sub u@(BVec _ _) (SVec _ _) = u


-- | `mul` elementwise multiplication of vectors

mul :: (Ord i, Ord v, Num v) =>
       SparseVector i v -> SparseVector i v -> SparseVector i v
-- Binary vectors
mul (BVec !ud !ui) (BVec _ !vi) = BVec ud (Set.intersection ui vi)
-- Sparse typed value vectors
mul (SVec !ud !ui) (SVec _ !vi) = SVec ud (mergeWith (*) ui vi)

-- TODO mixed arithmetic:
mul u@(SVec _ _) (BVec _ _) = u
mul u@(BVec _ _) (SVec _ _) = u

-- | Size or length of vector 
size :: SparseVector i v -> Int
size (SVec _ !ui) = Set.size ui
size (BVec _ !ui) = Set.size ui

-- | Dimensionality
dims :: Integral i => SparseVector i v -> i
dims (SVec !d _) = d
dims (BVec !d _) = d

-- | Density
density :: Integral i => SparseVector i v -> Double
density v@(SVec !ud _) = fromIntegral (size v) / fromIntegral ud
density v@(BVec !ud _) = fromIntegral (size v) / fromIntegral ud

-- | Similartiy/distance
sqdist :: (Ord i, Ord v, Num v) => SparseVector i v -> SparseVector i v -> v
sqdist u@(SVec _ _) v@(SVec _ _) =
  let dv = sub u v
      (SVec _ !v2) = mul dv dv
  in sum [x | (_,x) <- Set.toList v2]


-- this would be xor bitwise and much faster dense no doubt
distance :: Ord a => SparseVector a v -> SparseVector a v -> Int
distance (BVec _ !ui) (BVec _ !vi) = Set.size (Set.difference (Set.union ui vi)
                                               (Set.intersection ui vi))

-- TODO  or not?
-- sqdist (SVec _ _) (BVec _ _) = undefined
-- sqdist (BVec _ _) (SVec _ _) = undefined

-- | Normalised distance
difference :: Integral a => SparseVector a v -> SparseVector a v -> Double
difference !u !v = (fromIntegral $ distance u v) / (fromIntegral $ dims u)

-- | Inverse of difference
similarity :: Integral a => SparseVector a v -> SparseVector a v -> Double
similarity !u !v = 1.0 - difference u v 

-}

