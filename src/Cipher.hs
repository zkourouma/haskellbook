module Cipher
  ( caesar
  , unCaesar
  ) where

import           Data.Char

caesar :: Int -> String -> String
caesar offset = map shifty
  where
    shifty c =
      let f = mod (offset + ord c)
      in chr $
         if isUpper c
           then ord 'A' + mod (f (ord 'Z')) (ord 'A') + 1
           else ord 'a' + mod (f (ord 'z')) (ord 'a') + 1

unCaesar :: Int -> String -> String
unCaesar offset = caesar (-offset)
