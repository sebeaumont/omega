{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE BangPatterns #-}

-- | Its a breeze: Sparse, Random, Distributed memory etc. motto: keep
-- it simple and and explore the mathematical properties before
-- getting back to performance of the induced algebra herein. We are
-- really looking at the vector/metric space or more generally the
-- module over randomly distributed vectors.

module Breeze where

import qualified System.Random.MWC as Random
import Control.Monad.Reader

--import qualified Data.Vector as V -- might go unboxed and mmap for db in due course...
import qualified Data.SortedList as SL
import Crypto.Hash.SHA256 -- for later

type Gen = Random.GenIO

-- | Encapsulate underlying RNG
newtype RNG = RNG Gen

-- | Init the RNG and let the entropy flow
initRNG ::  IO RNG
initRNG = do RNG <$> Random.createSystemRandom

-- | We may want more here in due course...
data Environment = Env { envRNG :: RNG }


-- | thread access to sparse environment
newtype Entropy a = Entropy (ReaderT Environment IO a)
  deriving (Functor, Applicative, Monad, MonadIO, MonadReader Environment)

class (Monad m) => MonadEntropy m where
  liftEntropy :: Entropy a -> m a
  
instance MonadEntropy Entropy where
  liftEntropy = id


-- | Run actions in the context of Entropy monad - might make this more general to

withEntropy :: Entropy a -> IO a
withEntropy (Entropy s) = liftIO initRNG >>= \r -> runReaderT s (Env r) 
  

-- | The vector needs to be constrained to a type `Random.Variate` which ahs enough capacity
-- for the upper bound or dimension d.

getRandomList :: (MonadEntropy m, Random.Variate a, Integral a) => Int -> a -> m [a]
getRandomList n d = liftEntropy $ Entropy $ do
  RNG g <- asks envRNG
  replicateM n (Random.uniformR (0,d-1) g)

-- | Sparse vectors have a dimension and a list of indexes and values in classic
-- sparse vector form there are also binary/bit/boolean vectors which just have
-- the indexes and no value type storage.

data SparseVector i v = SVec !i !(SL.SortedList (i,v))
                      | BVec !i !(SL.SortedList i)
                      deriving (Show)


toSortedPairs :: (Ord a, Ord b) => [a] -> [b] -> SL.SortedList (a, b)
toSortedPairs i v = SL.toSortedList $ zip i v

toSortedInit :: (Ord v, Ord i) => v -> [i] -> SL.SortedList (i, v)
toSortedInit v i = toSortedPairs i (repeat v)

toZeros :: (Ord i, Ord v, Num v) => [i] -> SL.SortedList (i, v)
toZeros i = toSortedPairs i (repeat 0)


-- | sparse distirbuted vectors have a uniform probaility p of an index at given dimensionality

-- | p d useful for bit vectors existential on index but with no values
-- XXshould we force v type to be Void?

makeSparseRandomBitVector  :: (MonadEntropy m, Random.Variate a, Integral a) =>
                              Int -> a -> m (SparseVector a v)
makeSparseRandomBitVector p d = (BVec d . SL.toSortedList) <$> (getRandomList p d)


-- | Could make this a vector of vectors... (matrix) rather than a list
-- withEntropy $ makeSparseRandomBitVectors 10 16 16496 :: IO ([SparseVector Word16 Void])

makeSparseRandomBitVectors :: (MonadEntropy m, Random.Variate a, Integral a) =>
                              Int -> Int -> a -> m [SparseVector a v]
makeSparseRandomBitVectors n p d = replicateM n (makeSparseRandomBitVector p d)

-- | p d v=0 sparse random with values...
-- withEntropy $ makeSparseVector2 16 16392 :: IO (SparseVector2 Word16 Double)

makeSparseRandomVector  :: (MonadEntropy m, Random.Variate a, Integral a, Num v, Ord v) =>
                           Int -> a -> m (SparseVector a v)
makeSparseRandomVector p d = (SVec d . toZeros) <$> (getRandomList p d)

-- | Could make this a vector of vectors... (matrix) rather than a list
makeSparseRandomVectors :: (MonadEntropy m, Random.Variate a, Integral a, Num v, Ord v) =>
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


-- | Add two sparse vectors.  we are inclined to ignore the
-- dimensionality of one or other arguments here on the assumption
-- that it makes no sense for them to be different. I'm thinking that
-- the dimensionality of a sparse vector is really in the greatest
-- lower bound on the number of indexes existing and the capacity of
-- the index type and could be part of the evaluation semantics rather than
-- fixed in the type... we could take the max of course!

add :: (Ord i, Ord v, Num v) =>
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
  let fk = toZeros $ SL.fromSortedList vi
  in SVec ud (SL.toSortedList (unionWith (+) (SL.fromSortedList ui) (SL.fromSortedList fk)))
    
add (BVec _ !vi)  (SVec !ud !ui) =
  let fk = toZeros $ SL.fromSortedList vi
  in SVec ud (SL.toSortedList (unionWith (+) (SL.fromSortedList ui) (SL.fromSortedList fk)))

-- negate a vector

negatev :: (Ord i, Ord v, Num v) => SparseVector i v -> SparseVector i v
negatev (SVec d vs) = SVec d $ SL.map (\(i,v) -> (i, negate v)) vs
negatev u@(BVec _ _) = u

-- | Subtract -- under construction!

sub :: (Ord i, Ord v, Num v) =>
       SparseVector i v -> SparseVector i v -> SparseVector i v
-- Binary vectors take intersection
sub (BVec !ud !ui) (BVec _ !vi) = BVec ud (SL.intersect ui vi)
-- Sparse typed value vectors
sub u@(SVec _ _) v@(SVec _ _) = add u (negatev v)
-- TODO mixed arithmetic:


-- filter zero values? (truncate?)


-- Incremental union/merge of sorted lists of index pairs takes
-- binary function of values. N.B. due to non-greedy behaviour only
-- applies binary fn to `snd` of matched `fst` in either list so this
-- must be applied each time we wish to aggregate values as above.  So
-- for duplicated `fst` or indexes e.g. we would get:
--  `unionWith` (+) [(1,2),(1,3),(1,4)] [(1,2),(1,3),(1,4)] = [(1,4),(1,6),(1,8)]
--  rather than [(1.18)]

unionWith :: (Ord a, Num b) => (b -> b -> b) -> [(a, b)] -> [(a, b)] -> [(a, b)]
unionWith f p1@((i1,v1):r1) p2@((i2,v2):r2)
  | i1 > i2 = (i2, v2) : unionWith f p1 r2
  | i1 < i2 = (i1, v1) : unionWith f r1 p2
  | i1 == i2 = (i1, f v1 v2) : unionWith f r1 r2 
unionWith _ l []  = l  
unionWith _ [] l = l

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

-- | Subtraction

{-
-- | Similartiy/distance
sqdistance :: (Ord i, Ord v, Num v) => SparseVector i v -> SparseVector i v -> v
sqdistance u@(SVec _ uv) v@(SVec _ vv) = sub u v
-}

