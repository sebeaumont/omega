module Data.Text.Tokenize (tokens, frames) where

import qualified Data.Text.Lazy as T
import Data.Char (isPunctuation)

-- | Lightweight tokenizer

tokens :: T.Text -> [T.Text]
tokens s = map (T.toCaseFold . stripR . stripL) (T.words s)

stripR :: T.Text -> T.Text
stripR = T.dropWhileEnd isPunctuation

stripL :: T.Text -> T.Text
stripL = T.dropWhile isPunctuation

-- | We don't bother trying to segment sentences or any NLP thing
-- just create a 1-d convolution of the list of tokens
-- with a frame size or window `n` with overlap `o`
frames :: Int -> Int -> [a] -> [[a]]
frames n o l@(_:ts) =
  take n l : frames n o (drop (n-(o+1)) ts)
frames _ _ [] = []  
