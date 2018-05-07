module ChNine
  ( zzip
  , zzipWith
  , yell
  , yelling
  , titleize
  ) where

import           Data.Char

zzipWith :: (a -> b -> c) -> [a] -> [b] -> [c]
zzipWith fn [] _          = []
zzipWith fn _ []          = []
zzipWith fn (x:xs) (y:ys) = fn x y : zzipWith fn xs ys

zzip :: [a] -> [b] -> [(a, b)]
zzip = zzipWith (\a b -> (a, b))

yell :: String -> String
yell ""     = ""
yell (x:xs) = toUpper x : yell xs

yelling :: String -> String
yelling = filter isUpper

titleize :: String -> String
titleize ""     = ""
titleize (s:ss) = toUpper s : ss

cap :: String -> Char
cap = toUpper . head
