{-# LANGUAGE BangPatterns #-}

module Data.SDM.VectorSpace where

import Control.Monad
import Data.SDM.Entropy
import qualified Data.SortedList as SL
import qualified Data.Set as Set

---------------------------------------------------------------------------------------------
-- | Sparse vectors have a dimension and a list of indexes and values in classic
-- sparse vector form there are also binary/bit/boolean vectors which just have
-- the indexes and no value type storage.

data SparseVector i v = SVec !i !(SL.SortedList (i,v))
                      | BVec !i !(SL.SortedList i)
                      deriving (Show)

-- auxiliary constructors TODO look at using sets in stead of SortedLists...
-- if we can avoid converting back and forth too much

toSortedPairs :: (Ord a, Ord b) => [a] -> [b] -> SL.SortedList (a, b)
toSortedPairs i v = SL.toSortedList $ zip i v

toSortedInit :: (Ord v, Ord i) => v -> [i] -> SL.SortedList (i, v)
toSortedInit v i = toSortedPairs i (repeat v)

toZeros :: (Ord i, Ord v, Num v) => [i] -> SL.SortedList (i, v)
toZeros i = toSortedPairs i (repeat 0)


-- | sparse distirbuted vectors have a uniform probaility p of an index at given dimensionality
-- N.B. XXX duplicates may occur! need to fix this -- need a randomChoice function XXX

-- | p d useful for bit vectors existential on index but with no values
-- XX should we force v type to be Void?

makeSparseRandomBitVector  :: (MonadEntropy m, Variate a, Integral a) =>
                              Int -> a -> m (SparseVector a v)
makeSparseRandomBitVector p d = (BVec d . SL.toSortedList) <$> (getRandomList p d)


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
fromList d is vs = SVec d $ SL.nub . SL.toSortedList $ zip is vs 

-- | Construct non-random sparse vector from lists of indexes and values 
sVecFromList :: (Ord i, Num i, Ord v) => i -> [i] -> [v] -> SparseVector i v
sVecFromList = fromList

-- | Construct binary (index only) sparse vector from lists of indexes
bVecFromList :: (Ord i, Num i, Ord v) => i -> [i]  -> SparseVector i v
bVecFromList d is = BVec d $ SL.nub . SL.toSortedList $  is

-- todo make this work (and other constant intialisers) work like toZeros above...
zeros :: (Ord v, Num v) => [v]
zeros = repeat 0


-- | Add two sparse vectors.
--
-- we are inclined to ignore the dimensionality of one or other
-- arguments here on the assumption that it makes no sense for them to
-- be different. I'm thinking that the dimensionality of a sparse
-- vector is really in the greatest lower bound on the number of
-- indexes existing and the capacity of the index type and could be
-- part of the evaluation semantics rather than fixed in the
-- type... but we could take the max of course!

add :: (Ord i, Ord v, Num v) => -- XXXX FIXME Num v breaks down for binary vectors! XXXX
       SparseVector i v -> SparseVector i v -> SparseVector i v
-- Binary vectors
add (BVec !ud !ui) (BVec _ !vi) = BVec ud (SL.union ui vi)
-- Sparse typed value vectors
add (SVec !ud !ui) (SVec _ !vi) =
  let us = SL.fromSortedList ui
      vs = SL.fromSortedList vi
  in SVec ud (SL.toSortedList (unionWith (+) us vs))
  
-- mixed arithmetic:
-- toZeros seems to satisfy type checker... do these make much sense anyway?
add (SVec !ud !ui) (BVec _ !vi) =
  let zs = toZeros $ SL.fromSortedList vi
  in SVec ud (SL.toSortedList (unionWith (+) (SL.fromSortedList ui) (SL.fromSortedList zs)))
    
add (BVec _ !vi)  (SVec !ud !ui) =
  let zs = toZeros $ SL.fromSortedList vi
  in SVec ud (SL.toSortedList (unionWith (+) (SL.fromSortedList ui) (SL.fromSortedList zs)))

-- negate a vector

negatev :: (Ord i, Ord v, Num v) => SparseVector i v -> SparseVector i v
negatev (SVec d vs) = SVec d $ SL.map (\(i,v) -> (i, negate v)) vs
negatev u@(BVec _ _) = u

-- | Subtract -- under construction!

sub :: (Ord i, Ord v, Num v) =>
       SparseVector i v -> SparseVector i v -> SparseVector i v
-- Binary vectors take intersection
sub (BVec !ud !ui) (BVec _ !vi) =
  let us = SL.fromSortedList ui
      vs = SL.fromSortedList vi
  in BVec ud $ SL.toSortedList $ difference us vs
  
-- Sparse typed value vectors
sub u@(SVec _ _) v@(SVec _ _) = add u (negatev v)

-- TODO mixed arithmetic:
sub u@(SVec _ _) (BVec _ _) = u
sub u@(BVec _ _) (SVec _ _) = u

-- TODO filter zero values? (truncate?)

-- | set difference using sortedlist

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

-- | `mul` elementwise multiplication of vectors

mul :: (Ord i, Ord v, Num v) =>
       SparseVector i v -> SparseVector i v -> SparseVector i v
-- Binary vectors
mul (BVec !ud !ui) (BVec _ !vi) = BVec ud (SL.intersect ui vi)
-- Sparse typed value vectors
mul (SVec !ud !ui) (SVec _ !vi) =
  let us = SL.fromSortedList ui
      vs = SL.fromSortedList vi
  in SVec ud (SL.toSortedList (unionWith (*) us vs))
-- TODO mixed arithmetic:
mul u@(SVec _ _) (BVec _ _) = u
mul u@(BVec _ _) (SVec _ _) = u
  
-- Incremental union/merge of sorted lists of index pairs takes
-- binary function of values. N.B. due to non-greedy behaviour only
-- applies binary fn to `snd` of matched `fst` in either list so this
-- must be applied each time we wish to aggregate values as above.  So
-- for duplicated `fst` or indexes e.g. we would get:
--  `unionWith` (+) [(1,2),(1,3),(1,4)] [(1,2),(1,3),(1,4)] = [(1,4),(1,6),(1,8)]
--  rather than [(1,18)]

unionWith :: (Ord a, Num b) => (b -> b -> b) -> [(a, b)] -> [(a, b)] -> [(a, b)]
unionWith f p1@((i1,v1):r1) p2@((i2,v2):r2)
  | i1 > i2 = (i2, v2) : unionWith f p1 r2
  | i1 < i2 = (i1, v1) : unionWith f r1 p2
  | i1 == i2 = (i1, f v1 v2) : unionWith f r1 r2 
unionWith _ l []  = l  
unionWith _ [] l = l
unionWith _ _ _ = [] -- undefined?

-- | Size or length of vector 
size :: SparseVector i v -> Int
size (SVec _ !ui) = length ui
size (BVec _ !ui) = length ui

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
  in sum [x | (_,x) <- SL.fromSortedList v2]
-- .. 

{- TODO: dense representations? -}                        


