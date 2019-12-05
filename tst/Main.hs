{-# LANGUAGE OverloadedStrings #-}
module Main where

import Criterion.Main
import Data.SDM.Index

main :: IO ()
main = defaultMain [
  bench "index" $ whnfAppIO (indexFile "dat/shakespeare.txt" 10) 1
  ]
