module Data.String.Strip
  ( strip
  , hi
  ) where

import           Data.Char

strip :: String -> String
strip = dropWhile isSpace . reverse . dropWhile isSpace . reverse

hi :: String -> String
hi s = "hi " ++ s
