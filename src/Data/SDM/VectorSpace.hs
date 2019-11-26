{-# LANGUAGE BangPatterns #-}
module Data.SDM.VectorSpace where
-- | First pass on SparseVector algebra

import Control.Monad
import Data.SDM.Entropy
import qualified Data.Set as Set
import Data.Set (Set)

---------------------------------------------------------------------------------------------
-- | Sparse vectors have a dimension and a list of indexes and values in classic
-- sparse vector (i..,v) form there are also binary/bit/boolean vectors which just have
-- the indexes and no value type storage.

data SparseVector i v = SVec !i !(Set (i,v))
                      | BVec !i !(Set i)
                      deriving (Show)

-- need to rename this to something more meaningful: toIndexSet

toSortedPairs :: (Ord a, Ord b) => [a] -> [b] -> Set (a, b)
toSortedPairs i v = Set.fromList $ zip i v

toZeros :: (Ord i, Ord v, Num v) => [i] -> Set (i, v)
toZeros i = toSortedPairs i (repeat 0)


-- | sparse distirbuted vectors have a uniform probaility p of an index at given dimensionality
-- N.B. XXX duplicates may occur! need to fix this -- need a randomChoice function XXX

-- | p d useful for bit vectors existential on index but with no values
-- XX should we force v type to be Void?

makeSparseRandomBitVector  :: (MonadEntropy m, Variate a, Integral a) =>
                              Int -> a -> m (SparseVector a v)
makeSparseRandomBitVector p d = (BVec d . Set.fromList) <$> (getRandomList p d)


-- | Could make this a vector of vectors... (matrix) rather than a list
-- withEntropy $ makeSparseRandomBitVectors 10 16 16496 :: IO ([SparseVector Word16 Void])

makeSparseRandomBitVectors :: (MonadEntropy m, Variate a, Integral a) =>
                              Int -> Int -> a -> m [SparseVector a v]
makeSparseRandomBitVectors n p d = replicateM n (makeSparseRandomBitVector p d)

-- | p d v=0 sparse random with values...
-- withEntropy $ makeSparseVector2 16 16392 :: IO (SparseVector Word16 Double)

makeSparseRandomVector  :: (MonadEntropy m, Variate a, Integral a, Num v, Ord v) =>
                           Int -> a -> m (SparseVector a v)
makeSparseRandomVector p d = (SVec d . toZeros) <$> (getRandomList p d)

-- | Could make this a vector of vectors... (matrix) rather than a list
makeSparseRandomVectors :: (MonadEntropy m, Variate a, Integral a, Num v, Ord v) =>
                           Int -> Int -> a -> m [SparseVector a v]
makeSparseRandomVectors n p d = replicateM n (makeSparseRandomVector p d)

-- | Construct non-random sparse vector from lists of indexes and values 
fromList :: (Ord i, Num i, Ord v) => i -> [i] -> [v] -> SparseVector i v
fromList d is vs = SVec d $ Set.fromList $ zip is vs 

-- | Construct non-random sparse vector from lists of indexes and values 
sVecFromList :: (Ord i, Num i, Ord v) => i -> [i] -> [v] -> SparseVector i v
sVecFromList = fromList

-- | Construct binary (index only) sparse vector from lists of indexes
bVecFromList :: (Ord i, Num i, Ord v) => i -> [i]  -> SparseVector i v
bVecFromList d is = BVec d $ Set.fromList $  is

-- todo make this work (and other constant intialisers) work like toZeros above...
--zeros :: (Ord v, Num v) => [v]
--zeros = repeat 0

  
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

-- | Try and do merge f with sets
-- XXX TODO: could partition union of s1 s2 based on a and then reduce with f
{-# INLINE mergeWith #-}
mergeWith :: (Ord a, Ord b, Num b) => (b -> b -> b) -> Set(a,b) -> Set(a,b) -> Set(a,b)
mergeWith f s1 s2 =
  let l1 = Set.toAscList s1
      l2 = Set.toAscList s2
  in Set.fromList $ unionWith f l1 l2
  
-- | Add two sparse vectors.
--
-- we are inclined to ignore the dimensionality of one or other
-- arguments here on the assumption that it makes no sense for them to
-- be different. I'm thinking that the dimensionality of a sparse
-- vector is really in the interval greatest lower bound on the number of
-- indexes existing and the capacity of the index type and could be
-- part of the evaluation semantics rather than fixed in the
-- type... but we could take the max of course!

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

{-
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
-}

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

-- TODO filter zero values? (truncate?)


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



