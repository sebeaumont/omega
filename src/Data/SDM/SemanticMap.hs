{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE Strict #-}

module Data.SDM.SemanticMap where

import Control.DeepSeq (NFData)
import Control.Monad.IO.Class (liftIO)
import qualified Data.HashMap.Strict as Map
import Data.List
import Data.SDM.Entropy
import Data.SDM.SemanticVector
import Data.SDM.VectorSpace
import Data.Text.Lazy (Text)
import qualified Data.Text.Lazy.IO as TIO
import Data.Text.Tokenize

-- factor out some of this relevant function should be obvious from this newtype/c'tor usage below...
-- could use the same type to map to sparse vectors for graphs/hypergraphs...

-- | More powerful type for SemanticMap
newtype SemanticMap a = SemanticMap (Map.HashMap a SemanticVector)
  deriving (NFData)

-- | Map from term tokens to SemanticVectors
type TokenMap = SemanticMap Text

-- | Update map with vector returning updated map
updateVector :: TokenMap -> Text -> SemanticVector -> TokenMap
updateVector (SemanticMap m) k v = SemanticMap $ Map.insert k v m

updateVectors :: TokenMap -> [Text] -> [SemanticVector] -> TokenMap
updateVectors m (k : ks) (v : vs) =
  let m' = updateVector m k v in updateVectors m' ks vs
updateVectors m [] _ = m
updateVectors m _ [] = m

-- | Lookup token and return a SemanticVector either existing or new.
ensureSemanticVector :: MonadEntropy m => TokenMap -> Text -> m (SemanticVector, TokenMap)
ensureSemanticVector (SemanticMap tm) tok =
  case Map.lookup tok tm of
    Nothing -> makeSemanticVector >>= \sv -> return (sv, SemanticMap $ Map.insert tok sv tm)
    Just sv' -> return (sv', SemanticMap tm)

-- | Frame based indexing making sure updated map is used
frameVectors :: MonadEntropy m => TokenMap -> [Text] -> m [(SemanticVector, TokenMap)]
frameVectors tm (t : ts) = do
  (!v, !tm') <- ensureSemanticVector tm t
  !rs <- frameVectors tm' ts
  return $! (v, tm') : rs
frameVectors _ [] = return []

-- | SemanticMap a frame
indexFrame :: MonadEntropy m => TokenMap -> [Text] -> m TokenMap
indexFrame tm frame = do
  vectorMap <- frameVectors tm frame
  let vectors = map fst vectorMap
      tm' = snd . last $ vectorMap
      mv = mutual vectors
  -- now update the vectors with the mutual info
  return $ updateVectors tm' frame mv

-- | SemanticMap frames for side effects on TokenMap
indexFrames :: MonadEntropy m => TokenMap -> [[Text]] -> m TokenMap
indexFrames tm (f : fs) = do
  tm' <- indexFrame tm f
  rs <- indexFrames tm' fs
  return $! rs
indexFrames tm [] = return tm

-- | SemanticMap a text with given frame size and overlap
indexText :: MonadEntropy m => Text -> Int -> Int -> m TokenMap
indexText text fsize over =
  let fms = frames fsize over $ tokens text
   in indexFrames (SemanticMap Map.empty) fms

-- IO, IO it's off to work we go...

-- | e.g. SemanticMap tokenised text from stdin
indexStdin :: Int -> Int -> IO TokenMap
indexStdin sz ov =
  withEntropy $ do
    text <- liftIO TIO.getContents
    let fms = frames sz ov $ tokens text
    indexFrames (SemanticMap Map.empty) fms

indexFile :: FilePath -> Int -> Int -> IO TokenMap
indexFile fp sz ov =
  withEntropy $ do
    text <- liftIO $ TIO.readFile fp
    let fms = frames sz ov $ tokens text
    indexFrames (SemanticMap Map.empty) fms

-- | Select nearest neighbours with difference (normalised distance) below given threshold
neighbours :: TokenMap -> SemanticVector -> Int -> Int -> [(Text, Int)]
neighbours (SemanticMap m) !v !s !n =
  let !v' = sV v
      !namedist = sortOn snd [(t, distance (sV u) v') | (t, u) <- Map.toList m]
   in take n $ takeWhile (\(_, s') -> s' < s) namedist

-- | Lookup a token in the map
token :: TokenMap -> Text -> Maybe SemanticVector
token (SemanticMap m) !s = Map.lookup s m


-- | Convenience for lookups
neighbourhood :: TokenMap -> Text -> Double -> Int -> Maybe [(Text, Int)]
neighbourhood tm tok p n = do
  source <- token tm tok
  let j = round $ p * fromIntegral d
  return $ neighbours tm source j n 
  
