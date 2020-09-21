{-# LANGUAGE OverloadedStrings #-}
module Main where

import Criterion.Main
import Data.SDM.SemanticMap
import Data.SDM.SemanticVector

setupEnv :: IO (TokenMap, SemanticVector)
setupEnv = do 
  idx <- indexFile "dat/shakespeare.txt" 10 1
  let Just duchess = token idx "duchess"
  return (idx, duchess)
  
main :: IO ()
main = defaultMain [
  env setupEnv $ \ ~(idx, duchess) -> bgroup "main"
   [ bgroup "tokens" 
     [ bench "summer" $ whnf (flip token "summer") idx
     , bench "york" $ whnf (flip token "york") idx
     , bench "the" $ whnf (flip token "the") idx
     ]
   , bgroup "neighbours"
     [ bench "duchess" $ whnf (neighbours idx duchess 10000) 10]]]

