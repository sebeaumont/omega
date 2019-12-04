{-# LANGUAGE OverloadedStrings #-}
module Main where

import Criterion.Main
import Data.SDM.Index

--
-- NOTA BONA - This would rely on stuff being evaluable to NF
-- and thus implementing NFData for our main data strucutres.
-- which might be an option but I'm not feeling up to it yet.

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

