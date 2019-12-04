{-# LANGUAGE BangPatterns #-}
module Data.SDM.Index where

import Control.Monad.IO.Class (liftIO)

import Data.SDM.VectorSpace
import Data.SDM.SemanticVector
import Data.SDM.Entropy
import Data.List

import Data.Text.Lazy (Text)
import qualified Data.Text.Lazy as T
import qualified Data.Text.Lazy.IO as TIO
import Data.Text.Tokenize
import qualified Data.HashMap.Strict as Map

-- | Map from term tokens to SemanticVectors   
type TokenMap = Map.HashMap Text SemanticVector

-- | Update map with vector returning updated map
updateVector :: TokenMap -> Text -> SemanticVector -> TokenMap
updateVector m k v = Map.insert k v m

updateVectors :: TokenMap -> [Text] -> [SemanticVector] -> TokenMap
updateVectors m (k:ks) (v:vs) =
  let m' = updateVector m k v in updateVectors m' ks vs
updateVectors m [] _ = m
updateVectors m _ [] = m  

-- | Lookup token and return a SemanticVector either existing or new.
ensureSemanticVector :: MonadEntropy m => TokenMap -> Text -> m (SemanticVector, TokenMap)
ensureSemanticVector tm tok = 
  case Map.lookup tok tm of
    Nothing -> makeSemanticVector >>= \sv -> return (sv, Map.insert tok sv tm)
    Just sv' -> return (sv', tm)

-- | Frame based indexing making sure updated map is used
frameVectors :: MonadEntropy m => TokenMap -> [Text] -> m [(SemanticVector, TokenMap)]
frameVectors tm (t:ts) = do
  (v, tm') <- ensureSemanticVector tm t
  rs <- frameVectors tm' ts
  return $! (v, tm') : rs
frameVectors _ [] = return []

-- | Index a frame
indexFrame :: MonadEntropy m => TokenMap -> [Text] -> m TokenMap
indexFrame tm frame = do
  vectorMap <- frameVectors tm frame
  let vectors = map fst vectorMap
      tm' = snd . last $ vectorMap
      mv = mutual vectors
      -- now update the vectors with the mutual info
  return $ updateVectors tm' frame mv 

-- | Index frames for side effects on TokenMap
indexFrames :: MonadEntropy m => TokenMap -> [[Text]] -> m TokenMap
indexFrames tm (f:fs) = do
  tm' <- indexFrame tm f
  rs <- indexFrames tm' fs
  return $! rs
indexFrames tm [] = return tm

-- | Index a text with given frame size and overlap
indexText :: MonadEntropy m => Text -> Int -> Int -> m TokenMap
indexText text fsize over =
  let fms = frames fsize over $ tokens text
  in indexFrames Map.empty fms

-- IO, IO it's off to work we go...
-- | e.g. Index tokenised text from stdin
indexStdin :: Int -> Int -> IO TokenMap  
indexStdin sz ov = 
  withEntropy $ do
    text <- liftIO TIO.getContents
    let fms = frames sz ov $ tokens text
    indexFrames Map.empty fms

indexFile :: FilePath -> Int -> Int -> IO TokenMap
indexFile fp sz ov =
  withEntropy $ do
    text <- liftIO $ TIO.readFile fp
    let fms = frames sz ov $ tokens text
    indexFrames Map.empty fms
    
-- | Select nearest neighbours with difference (normalised distance) below given threshold
neighbours :: TokenMap -> SemanticVector -> Int -> Int -> [(Text, Int)]
neighbours !m !v !s !n =
  let !v' = sV v
      !namedist = sortOn snd [(t, distance (sV u) v') | (t,u) <- Map.toList m]
  in
    take n $ takeWhile (\(_,s') -> s' < s) namedist

-- | Lookup a token in the map
token :: TokenMap -> Text -> Maybe SemanticVector
token m s = Map.lookup s m

