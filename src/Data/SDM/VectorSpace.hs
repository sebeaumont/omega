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
                            , withDims
                            , dimensions
                            , MonadSpace
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
  

