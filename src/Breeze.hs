{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE BangPatterns #-}

module Breeze where

import qualified System.Random.MWC as Random
import Control.Monad.Reader
--import qualified Data.Vector as V
import Data.Word
import qualified Data.SortedList as SL

type Gen = Random.GenIO

-- | Encapsulate underlying RNG
newtype RNG = RNG Gen

-- | Init the RNG and let the entropy flow
initRNG ::  IO RNG
initRNG = do
  rng <- Random.createSystemRandom
  return (RNG rng)

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


-- | Sparse vectors have dimension and a list of indexes but no values yet, but could be pairs
-- with the indexes.
{-
--data SparseVector a = SVec a (SortedList a) deriving (Show)
data SparseVector a = SVec { dim_ :: !a
                           , idx_ :: !(SL.SortedList a)
                           } deriving (Show)
-}

-- with values...
data SparseVector i v = SVec { dim2_ :: !i
                             , idx2_ :: !(SL.SortedList (i,v))
                             }
                      | BVec { dim_ :: !i
                             , idx_ :: !(SL.SortedList i)
                             }
                      deriving (Show)


toSortedPairs :: (Ord a, Ord b) => [a] -> [b] -> SL.SortedList (a, b)
toSortedPairs i v = SL.toSortedList $ zip i v

toSortedInit :: (Ord v, Ord i) => v -> [i] -> SL.SortedList (i, v)
toSortedInit v i = toSortedPairs i (repeat v)

toZeros :: (Ord i, Ord v, Num v) => [i] -> SL.SortedList (i, v)
toZeros i = toSortedPairs i (repeat 0)


-- sparse distirbuted vectors have a uniform probaility p of an index at given value of dim_
-- | p d useful for bit vectors existential on index but with no values

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


-- | Add two sparse vectors
-- XXX no dim check, no dependent types.

add :: (Ord i, Ord v, Num v) => SparseVector i v -> SparseVector i v -> SparseVector i v
add (BVec !ud !ui) (BVec !vd !vi) = BVec ud (SL.union ui vi)
add (SVec !ud !ui) (SVec !vd !vi) =
  let us = SL.fromSortedList ui
      vs = SL.fromSortedList vi
  in SVec ud (SL.toSortedList (unionAdd us vs))

  
-- | Trixy little pattern match to merge (add) sorted index pairs
unionAdd :: (Ord a, Num b) => [(a, b)] -> [(a, b)] -> [(a, b)]
unionAdd p1@((i1,v1):r1) p2@((i2,v2):r2)
  | i1 > i2 = (i2, v2) : unionAdd p1 r2
  | i1 < i2 = (i1, v1) : unionAdd r1 p2
  | i1 == i2 = (i1, v1+v2) : unionAdd r1 r2
unionAdd [] l@(x:xs) = l  
unionAdd l@(x:xs) [] = l
unionAdd [] [] = []

-- does it make sense to add mixed?
{-
size :: SparseVector a -> Int
size (SVec _ !ui) = length ui

density :: Integral a => SparseVector a -> Double
density v@(SVec !ud _) = fromIntegral (size v) / fromIntegral ud
-}


