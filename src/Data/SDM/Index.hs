module Data.SDM.Index where

import Control.Monad.IO.Class (liftIO)
import Data.List
import Data.SDM.VectorSpace
import Data.SDM.Entropy
--import Data.Void
import qualified Data.Text.Lazy as T
import qualified Data.Text.Lazy.IO as TIO
import Data.Text.Tokenize
import qualified Data.HashMap.Strict as Map
-- not yet Rosin, not yet: import Grenade

{-

Explore the use case where we use SDM to train the embedding of some
text and use this as input to a RNN (LSTM) and see how performance
differs for a 1-hot representation.


 1. Create test pipeline. In due course we really need a predictive model to measure?

-- Use shakespeare example
-- Pre train semantic space...
-- downsample SV's?
-- define LSTM network 
-- send to input layer of LSTM

TODO: parameretize vector constructors for d and p

-}

-- try sparse bits embedding
-- type Binary = Void -- XXX FIXME VectorSpace.SparseVector

p :: Int
p = 16

d :: Int
d = 32768

-- | Let's try SparseBinary embedding
type Binary = Int

type SparseBinaryVector = SparseVector Int Binary

data SemanticVector = SV { sK :: SparseBinaryVector
                         , sV :: SparseBinaryVector
                         } deriving (Show)

-- | Superpose SemanticVector u with sparse vector
super :: SemanticVector -> SparseBinaryVector -> SemanticVector
super u sv = u { sV = add sv (sV u) }

-- | NOTA superposition is associative (Monoid) => this can be fused
-- and we can batch superpose a frame...
-- if we dont mind reflection we can sum up all the sparse vectors
-- in a frame and then do only O(2n) vector adds

mutual :: [SemanticVector] -> [SemanticVector]
mutual vs = [super u mv | u <- vs] where
  mv = let zv = bVecFromList d [] in
         foldl' add zv [sK v | v <- vs] 

-- | Map from term tokens to SemanticVectors   
type TokenMap = Map.HashMap T.Text SemanticVector

-- | Update map with vector returning updated map
updateVector :: TokenMap -> T.Text -> SemanticVector -> TokenMap
updateVector m k v = Map.insert k v m

updateVectors :: TokenMap -> [T.Text] -> [SemanticVector] -> TokenMap
updateVectors m (k:ks) (v:vs) =
  let m' = updateVector m k v in updateVectors m' ks vs
updateVectors m [] _ = m
updateVectors m _ [] = m  

-- | Make a new SemanticVector - requires entropy for random number generation.
makeSemanticVector :: MonadEntropy m => m SemanticVector
makeSemanticVector = do
  svK' <- makeSparseRandomBitVector p d
  let svV' = bVecFromList d []
  return $ SV svK' svV'

-- | Lookup token and return a SemanticVector either existing or new.
ensureSemanticVector :: MonadEntropy m => TokenMap -> T.Text -> m (SemanticVector, TokenMap)
ensureSemanticVector tm tok = 
  case Map.lookup tok tm of
    Nothing -> makeSemanticVector >>= \sv -> return (sv, Map.insert tok sv tm)
    Just sv' -> return (sv', tm)

-- | Frame based indexing making sure updated map is used
frameVectors :: MonadEntropy m => TokenMap -> [T.Text] -> m [(SemanticVector, TokenMap)]
frameVectors tm (t:ts) = do
  (v, tm') <- ensureSemanticVector tm t
  rs <- frameVectors tm' ts
  return $! (v, tm') : rs
frameVectors _ [] = return []

-- | Index a frame
indexFrame :: MonadEntropy m => TokenMap -> [T.Text] -> m TokenMap
indexFrame tm frame = do
  vectorMap <- frameVectors tm frame
  let vectors = map fst vectorMap
      tm' = snd . last $ vectorMap
      mv = mutual vectors
      -- now update the vectors with the mutual info
  return $ updateVectors tm' frame mv 

-- | Index frames for side effects on TokenMap
indexFrames :: MonadEntropy m => TokenMap -> [[T.Text]] -> m TokenMap
indexFrames tm (f:fs) = do
  tm' <- indexFrame tm f
  rs <- indexFrames tm' fs
  return $! rs
indexFrames tm [] = return tm


-- | Index (lazy) text with given frame size and overlap
indexText :: MonadEntropy m => T.Text -> Int -> Int -> m TokenMap
indexText text fsize over =
  let fms = frames fsize over $ tokens text
  in indexFrames Map.empty fms


-- | e.g. Index tokenised text from stdin
indexStdin :: IO ()  
indexStdin = do
  withEntropy $ do
    text <- liftIO $ TIO.getContents
    let fms = frames 20 2 $ tokens text
    tm <- indexFrames Map.empty fms
    -- nothing really happens until here...
    let densities = [(k, density $ sV v) | (k,v) <- Map.toList tm] 
    liftIO $ print densities
