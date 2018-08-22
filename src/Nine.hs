module Nine where

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

-- Deliberately recursive
myOr :: [Bool] -> Bool
myOr []     = False
myOr (x:xs) = x || myOr xs

myAny :: (a -> Bool) -> [a] -> Bool
myAny _ []     = False
myAny f (x:xs) = f x || myAny f xs

myElem :: Eq a => a -> [a] -> Bool
myElem el = any (== el)

myReverse :: [a] -> [a]
myReverse []     = []
myReverse (x:xs) = myReverse xs ++ [x]

squish :: [[a]] -> [a]
squish []     = []
squish (x:xs) = x ++ squish xs

squishMap :: (a -> [b]) -> [a] -> [b]
squishMap _ []     = []
squishMap f (x:xs) = f x ++ squishMap f xs

-- Implement using squishMap
squishAgain :: [[a]] -> [a]
squishAgain = squishMap id

myMaximumBy :: (a -> a -> Ordering) -> [a] -> a
myMaximumBy _ [x] = x
myMaximumBy f [x, y] =
  case f x y of
    LT -> y
    _  -> x
myMaximumBy f (x:y:xs) =
  let keeper =
        case f x y of
          LT -> y
          _  -> x
   in myMaximumBy f (keeper : xs)

myMinimumBy :: (a -> a -> Ordering) -> [a] -> a
myMinimumBy _ [x] = x
myMinimumBy f [x, y] =
  case f x y of
    LT -> x
    _  -> y
myMinimumBy f (x:y:xs) =
  let keeper =
        case f x y of
          LT -> x
          _  -> y
   in myMinimumBy f (keeper : xs)

myMaximum :: (Ord a) => [a] -> a
myMaximum = myMaximumBy compare

myMinimum :: (Ord a) => [a] -> a
myMinimum = myMinimumBy compare
