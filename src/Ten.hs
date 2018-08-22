module Ten where

import           Data.Time

data DatabaseItem
  = DbString String
  | DbNumber Integer
  | DbDate UTCTime
  deriving (Eq, Ord, Show)

theDatabase :: [DatabaseItem]
theDatabase =
  [ DbDate (UTCTime (fromGregorian 1911 5 1) (secondsToDiffTime 34123))
  , DbNumber 9001
  , DbString "hello, world"
  , DbDate (UTCTime (fromGregorian 1921 5 1) (secondsToDiffTime 34123))
  ]

filterDbDate :: [DatabaseItem] -> [UTCTime]
filterDbDate = foldr dates []
  where
    dates a b =
      case a of
        DbDate utc -> utc : b
        _          -> b

filterDbNumber :: [DatabaseItem] -> [Integer]
filterDbNumber = foldr numbers []
  where
    numbers a b =
      case a of
        DbNumber n -> n : b
        _          -> b

mostRecent :: [DatabaseItem] -> UTCTime
mostRecent = foldr f (UTCTime (fromGregorian 0 1 1) (secondsToDiffTime 0))
  where
    f a b =
      case a of
        DbDate utc -> max utc b
        _          -> b

sumDb :: [DatabaseItem] -> Integer
sumDb = sum . filterDbNumber

avgDb :: [DatabaseItem] -> Double
avgDb d =
  let s = sumDb d
      c = length (filterDbNumber d)
   in fromIntegral s / fromIntegral c

fibs :: [Integer]
fibs = 1 : scanl (+) 1 fibs

fibsN :: Int -> Integer
fibsN n = fibs !! n

fibs20 :: [Integer]
fibs20 = take 20 $ 1 : scanl (+) 1 fibs20

fibsLt100 :: [Integer]
fibsLt100 = takeWhile (< 100) $ 1 : scanl (+) 1 fibsLt100

factorial :: [Integer]
factorial = scanl (*) 1 [1 ..]

myOr :: [Bool] -> Bool
myOr = foldr (||) False

myAny :: (a -> Bool) -> [a] -> Bool
myAny f = foldr (\x y -> f x || y) False

myElem :: Eq a => a -> [a] -> Bool
myElem el = foldr (\x y -> el == x || y) False

myReverse :: [a] -> [a]
myReverse = foldl (flip (:)) []

myMap :: (a -> b) -> [a] -> [b]
myMap f = foldr (\x y -> f x : y) []

myFilter :: (a -> Bool) -> [a] -> [a]
myFilter fil =
  foldr
    (\x y ->
       if fil x
         then x : y
         else y)
    []

squish :: [[a]] -> [a]
squish = foldr (++) []

squishMap :: (a -> [b]) -> [a] -> [b]
squishMap fn = foldr (\x y -> fn x ++ y) []

squishAgain :: [[a]] -> [a]
squishAgain = squishMap id

myMaximumBy :: (a -> a -> Ordering) -> [a] -> a
myMaximumBy fn myList = foldr maxed (head myList) myList
  where
    maxed x y =
      case fn x y of
        GT -> x
        _  -> y

myMinimumBy :: (a -> a -> Ordering) -> [a] -> a
myMinimumBy fn myList = foldr minned (head myList) myList
  where
    minned x y =
      case fn x y of
        GT -> y
        _  -> x
