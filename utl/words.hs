{-# LANGUAGE OverloadedStrings #-}
module Main where

import qualified Data.Text.Lazy as T
import qualified Data.Text.Lazy.IO as TIO 
import Data.Text.Tokenize

main :: IO ()
main = TIO.interact wordList

wordList :: T.Text -> T.Text
wordList = T.intercalate "\n" . tokens
