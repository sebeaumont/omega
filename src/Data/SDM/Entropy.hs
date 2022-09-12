{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

-- | This random index generator is central to SDM.  We might consider
-- different distributions but a uniform choice modulo the
-- dimensionality of the space is the current implmentation for sparse
-- random vectors.

{-
TODO
 - (uniform) randomChoice from index set [0..dim)
 - balanced/white domain value sets e.g. {0,-1,0}
 - N.B. getRandomList is the rate limiting step so performance of this is critical
-}

module Data.SDM.Entropy 
  ( withEntropy
  , getRandomList
  , MonadEntropy
  , Random.Variate
  ) where

import qualified System.Random.MWC as Random
import Control.Monad.Reader
    ( replicateM, asks, MonadIO(..), MonadReader, ReaderT(..) )

-- | System random seeded PRNG
type Gen = Random.GenIO

-- | Encapsulate underlying RNG
newtype RNG = RNG Gen

-- | Init the RNG and let the entropy flow
initRNG ::  IO RNG
initRNG = RNG <$> Random.createSystemRandom

-- | We may want more here in due course... meanwhile use newtype
newtype Environment = Env { envRNG :: RNG }


-- | Computation with access to current state of PRNG 
newtype Entropy a = Entropy (ReaderT Environment IO a)
  deriving (Functor, Applicative, Monad, MonadIO, MonadReader Environment)

class (Monad m) => MonadEntropy m where
  liftEntropy :: Entropy a -> m a
  
instance MonadEntropy Entropy where
  liftEntropy = id

-- | Run actions in the context of Entropy monad
-- using the system random generator

withEntropy :: Entropy a -> IO a
withEntropy (Entropy s) = liftIO initRNG >>= \r -> runReaderT s (Env r) 
  
-- | The list of `n` random element needs to be constrained to a type `Random.Variate` which has enough capacity
-- for the upper bound (sparse dimension) `d`.

getRandomList :: (MonadEntropy m, Random.Variate a, Integral a) => Int -> a -> m [a]
getRandomList !n !d = liftEntropy $ Entropy $ do
  RNG !g <- asks envRNG
  replicateM n (Random.uniformR (0,d-1) g)
