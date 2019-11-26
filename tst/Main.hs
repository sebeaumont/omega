{-# LANGUAGE OverloadedStrings #-}
module Main where

import Data.SDM.Index

main :: IO ()
main = do
  tm <- indexFile "dat/shakespeare.txt" 20 2
  let Just york = token tm "york"
  mapM_ print (neighbours tm york 0.33)
