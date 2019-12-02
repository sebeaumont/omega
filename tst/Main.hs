{-# LANGUAGE OverloadedStrings #-}
module Main where

import Data.SDM.Index

main :: IO ()
main = do
  tm <- indexFile "dat/shakespeare.txt" 10 1
  let Just york = token tm "york"
      Just summer = token tm "summer"
  mapM_ print (neighbours tm york 10000 20)
  mapM_ print (neighbours tm summer 10000 20)
  --print york
