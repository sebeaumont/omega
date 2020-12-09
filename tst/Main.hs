{-# LANGUAGE OverloadedStrings #-}
module Main where

import Criterion.Main
import Data.SDM.SemanticMap

main :: IO ()
main = defaultMain
  [ bench "shakes-10-1" $ whnfAppIO (indexFile "dat/shakespeare.txt" 10) 1
  -- , bench "shakes-20-4" $ whnfAppIO (indexFile "dat/shakespeare.txt" 20) 4
  ]
