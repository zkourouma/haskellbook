module Applicatives where

import           Control.Applicative
import           Data.List                (elemIndex)
import           Test.QuickCheck
import           Test.QuickCheck.Checkers
import           Test.QuickCheck.Classes

-------- Applicative laws --------
--
-- Identity
-- pure id <*> v = v
--
-- Composition
-- pure (.) <*> u <*> v <*> w = u <*> (v <*> w)
--
-- Homomorphism
-- pure f <*> pure x = pure (f x)
--
-- Interchange
-- u <*> pure y = pure ($ y) <*> u
--
added :: Maybe Integer
added = Just (+ 3) <*> lookup 3 (zip [1, 2, 3] [4, 5, 6])

y0 :: Maybe Integer
y0 = lookup 3 $ zip [1, 2, 3] [4, 5, 6]

z0 :: Maybe Integer
z0 = lookup 2 $ zip [1, 2, 3] [4, 5, 6]

tupled :: Maybe (Integer, Integer)
tupled = (,) <$> y0 <*> z0

x1 :: Maybe Int
x1 = elemIndex 3 [1, 2, 3, 4, 5]

y1 :: Maybe Int
y1 = elemIndex 4 [1, 2, 3, 4, 5]

max' :: Int -> Int -> Int
max' = max

maxed :: Maybe Int
maxed = max' <$> x1 <*> y1

xs2 = [1, 2, 3]

ys2 = [4, 5, 6]

x2 :: Maybe Integer
x2 = lookup 3 $ zip xs2 ys2

y2 :: Maybe Integer
y2 = lookup 2 $ zip xs2 ys2

summed :: Maybe Integer
summed = pure sum <*> ((,) <$> x2 <*> y2)

newtype Identity a =
  Identity a
  deriving (Eq, Ord, Show)

instance Functor Identity where
  fmap f (Identity a) = Identity (f a)

instance Applicative Identity where
  pure = Identity
  (<*>) (Identity f) (Identity a) = Identity (f a)

newtype Constant a b = Constant
  { getConstant :: a
  } deriving (Eq, Ord, Show)

instance Functor (Constant a) where
  fmap _ (Constant a) = Constant a

instance Monoid a => Applicative (Constant a) where
  pure a = Constant mempty
  (<*>) (Constant f) (Constant _) = Constant f

data List a
  = Nil
  | Cons a
         (List a)
  deriving (Eq, Show)

instance Functor List where
  fmap _ Nil         = Nil
  fmap f (Cons a as) = Cons (f a) (fmap f as)

instance Applicative List where
  pure x = Cons x Nil
  (<*>) Nil _          = Nil
  (<*>) _ Nil          = Nil
  (<*>) (Cons x xs) ys = (x <$> ys) `append` (xs <*> ys)

instance Arbitrary a => Arbitrary (List a) where
  arbitrary = pure <$> arbitrary

append :: List a -> List a -> List a
append Nil ys         = ys
append (Cons x xs) ys = Cons x $ xs `append` ys

fold :: (a -> b -> b) -> b -> List a -> b
fold _ b Nil        = b
fold f b (Cons h t) = f h (fold f b t)

concat' :: List (List a) -> List a
concat' = fold append Nil

flatMap :: (a -> List b) -> List a -> List b
flatMap f as = concat' $ fmap f as

instance Eq a => EqProp (List a) where
  (=-=) = eq

take' :: Int -> List a -> List a
take' _ Nil = Nil
take' n (Cons x xs)
  | n > 0 = Cons x (take' (n - 1) xs)
  | otherwise = Cons x xs

repeat' :: a -> List a
repeat' x =
  let c = Cons x c
   in c

zip' :: List (a -> b) -> List a -> List b
zip' (Cons x xs) (Cons y ys) = Cons (x y) (zip' xs ys)
zip' _ _                     = Nil

newtype ZipList' a =
  ZipList' (List a)
  deriving (Eq, Show)

instance Functor ZipList' where
  fmap f (ZipList' xs) = ZipList' $ fmap f xs

instance Applicative ZipList' where
  pure x = ZipList' $ repeat' x
  (<*>) (ZipList' xs) (ZipList' ys) = ZipList' $ zip' xs ys

instance Arbitrary a => Arbitrary (ZipList' a) where
  arbitrary = pure <$> arbitrary

instance Eq a => EqProp (ZipList' a) where
  xs =-= ys = xs' `eq` ys'
    where
      xs' =
        let (ZipList' l) = xs
         in take' 1000 l
      ys' =
        let (ZipList' l) = ys
         in take' 1000 l
