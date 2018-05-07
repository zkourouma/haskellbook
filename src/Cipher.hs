module Cipher
  ( caesar
  , unCaesar
  ) where

import           Data.Char

caesar :: Int -> String -> String
caesar offset = map shifty
  where
    shifty c =
      let shifted = (offset + ord c)
      in chr $
         if isUpper c
           then caesar' (ord 'A') (ord 'Z') shifted
           else caesar' (ord 'a') (ord 'z') shifted

caesar' lower upper val
  | val > upper = lower + mod val (1 + upper)
  | val < lower = val - lower + upper + 1
  | otherwise = val

unCaesar :: Int -> String -> String
unCaesar offset = caesar $ negate offset
