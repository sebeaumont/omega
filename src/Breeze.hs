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

--data SparseVector a = SVec a (SortedList a) deriving (Show)
data SparseVector a = SVec { dim_ :: !a
                           , idx_ :: !(SL.SortedList a)
                           } deriving (Show)

-- with values...
data SparseVector2 i v = SVec2 { dim2_ :: !i
                               , idx2_ :: !(SL.SortedList (i,v))
                               } deriving (Show)

-- sparse distirbuted vectors have a uniform probaility p of an index at given value of dim_

{-
instance Functor SortedList where
  fmap = Data.SortedList.map
-}



toSortedPairs :: (Ord a, Ord b) => [a] -> [b] -> SL.SortedList (a, b)
toSortedPairs i v = SL.toSortedList $ zip i v

toSortedInit :: (Ord v, Ord i) => v -> [i] -> SL.SortedList (i, v)
toSortedInit v i = toSortedPairs i (repeat v)

toZeros :: (Ord i, Ord v, Num v) => [i] -> SL.SortedList (i, v)
toZeros i = toSortedPairs i (repeat 0)


-- These are the public intefaces...

-- | Sparse random vectors

makeSparseRandomVector  :: (MonadEntropy m, Random.Variate a, Integral a) =>
                     Int -> a -> m (SparseVector a)
makeSparseRandomVector p d = (SVec d . SL.toSortedList) <$> (getRandomList p d)


makeSparseRandomVector2  :: (MonadEntropy m, Random.Variate a, Integral a, Num v, Ord v) =>
                      Int -> a -> m (SparseVector2 a v)
makeSparseRandomVector2 p d = (SVec2 d . toZeros) <$> (getRandomList p d)


-- | Could make this a vector of vectors... (matrix) rather than a list
makeSparseRandomVectors :: (MonadEntropy m, Random.Variate a, Integral a) =>
                     Int -> Int -> a -> m [SparseVector a]
makeSparseRandomVectors n p d = replicateM n (makeSparseRandomVector p d)


-- XXX shoudn't Ord/Integral constraint be on SparseVector type?
-- for which we'd need a GADT e.g. is this a good idea?
{-
data SparseVector where
  SVec :: Integral a => !a -> !SortedList a
-} 

-- | Add two sparse vectors
-- XXX no dim check, no dependent types.
 
add :: Ord a => SparseVector a -> SparseVector a -> SparseVector a
add (SVec !ud !ui) (SVec !vd !vi) = SVec ud (SL.union ui vi)

size :: SparseVector a -> Int
size (SVec _ !ui) = length ui

density :: Integral a => SparseVector a -> Double
density v@(SVec !ud _) = fromIntegral (size v) / fromIntegral ud



