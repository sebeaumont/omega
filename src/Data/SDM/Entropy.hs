{-# LANGUAGE GeneralizedNewtypeDeriving #-}

-- | SDM the random index generator is central
-- we might consider different distributions but a uniform choice
-- modulo the dimensionality of the space is the current implmentation.

{-
TODO
 - (uniform) randomChoice from index set [0..dim)
 - balanced/white domain value sets e.g. {0,-1,0}
-}

module Data.SDM.Entropy ( withEntropy
                        , getRandomList
                        , MonadEntropy
                        , Random.Variate
                        ) where

import qualified System.Random.MWC as Random
import Control.Monad.Reader

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

-- | Run actions in the context of Entropy monad

withEntropy :: Entropy a -> IO a
withEntropy (Entropy s) = liftIO initRNG >>= \r -> runReaderT s (Env r) 
  
-- | The vector needs to be constrained to a type `Random.Variate` which ahs enough capacity
-- for the upper bound or dimension d.

getRandomList :: (MonadEntropy m, Random.Variate a, Integral a) => Int -> a -> m [a]
getRandomList n d = liftEntropy $ Entropy $ do
  RNG g <- asks envRNG
  replicateM n (Random.uniformR (0,d-1) g)
