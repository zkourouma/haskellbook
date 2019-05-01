module Monadic where

import           Control.Monad            (join, liftM2)
import           Test.QuickCheck          (Arbitrary, arbitrary)
import           Test.QuickCheck.Checkers
import           Test.QuickCheck.Classes

--
-------- Monad laws --------
--
-- Right Identity --
-- m >>= return   = m
--
-- Left Identity --
-- return x >>= f = f x
--
-- Associativity --
-- (m >>= f) >>= g = m >>= (\x -> f x >>= g)
--
-- return :: Monad m => a -> m a
-- (>>=) :: Monad m => m a -> (a -> m b) -> m b -- "bind"
-- (=<<) :: Monad m => (a -> m b) -> m a -> m b -- "flip bind"
-- (>>) :: Monad m => m a -> m b -> m b -- sequential composition
-- mapM :: Monad m => (a -> m b) -> [a] -> m [b]
-- join :: Monad m => m (m a) -> m a
--
{-# ANN bind "HLint: ignore" #-}

bind :: Monad m => (a -> m b) -> m a -> m b
bind f m = join $ f <$> m

data Sum a b
  = First a
  | Second b
  deriving (Eq, Show)

instance Functor (Sum a) where
  fmap _ (First a)  = First a
  fmap f (Second b) = Second (f b)

instance Applicative (Sum a) where
  pure = Second
  (<*>) (First a) _           = First a
  (<*>) (Second a) (Second b) = Second (a b)

instance Monad (Sum a) where
  return = pure
  (>>=) (First a) _  = First a
  (>>=) (Second b) f = f b

data Nope a =
  NopeDotJpg
  deriving (Eq, Show)

instance Functor Nope where
  fmap _ NopeDotJpg = NopeDotJpg

instance Applicative Nope where
  pure _ = NopeDotJpg
  (<*>) _ _ = NopeDotJpg

instance Monad Nope where
  return = pure
  (>>=) _ _ = NopeDotJpg

instance Arbitrary a => Arbitrary (Nope a) where
  arbitrary = pure <$> arbitrary

instance Eq a => EqProp (Nope a) where
  (=-=) = eq

data PhhhbbtttEither b a
  = Left a
  | Right b
  deriving (Eq, Show)

instance Functor (PhhhbbtttEither b) where
  fmap f (Monadic.Left a)  = Monadic.Left (f a)
  fmap _ (Monadic.Right b) = Monadic.Right b

instance Applicative (PhhhbbtttEither b) where
  pure = Monadic.Left
  (<*>) (Monadic.Left a) (Monadic.Left a') = Monadic.Left (a a')
  (<*>) (Monadic.Right b) _                = Monadic.Right b

instance Monad (PhhhbbtttEither b) where
  return = pure
  (>>=) (Monadic.Left a) f = f a

instance Arbitrary a => Arbitrary (PhhhbbtttEither b a) where
  arbitrary = pure <$> arbitrary

instance (Eq a, Eq b) => EqProp (PhhhbbtttEither b a) where
  (=-=) = eq

newtype Identity a =
  Identity a
  deriving (Eq, Ord, Show)

instance Functor Identity where
  fmap f (Identity a) = Identity (f a)

instance Applicative Identity where
  pure = Identity
  (<*>) (Identity f) (Identity a) = Identity (f a)

instance Monad Identity where
  return = pure
  (>>=) (Identity a) f = f a

instance Arbitrary a => Arbitrary (Identity a) where
  arbitrary = pure <$> arbitrary

instance Eq a => EqProp (Identity a) where
  (=-=) = eq

data List a
  = Nil
  | Cons a
         (List a)
  deriving (Eq, Show)

append :: List a -> List a -> List a
append Nil as         = as
append (Cons a as) bs = Cons a $ as `append` bs

instance Functor List where
  fmap _ Nil         = Nil
  fmap f (Cons a as) = Cons (f a) (fmap f as)

instance Applicative List where
  pure a = Cons a Nil
  (<*>) Nil _          = Nil
  (<*>) _ Nil          = Nil
  (<*>) (Cons a as) bs = (a <$> bs) `append` (as <*> bs)

instance Monad List where
  return = pure
  (>>=) Nil _         = Nil
  (>>=) (Cons a as) f = f a

instance Arbitrary a => Arbitrary (List a) where
  arbitrary = pure <$> arbitrary

instance Eq a => EqProp (List a) where
  (=-=) = eq

j :: Monad m => m (m a) -> m a
j = join

l1 :: Monad m => (a -> b) -> m a -> m b
l1 = fmap

l2 :: Monad m => (a -> b -> c) -> m a -> m b -> m c
l2 = liftM2

a :: Monad m => m a -> m (a -> b) -> m b
a = flip (<*>)

meh :: Monad m => [a] -> (a -> m b) -> m [b]
meh [] _     = return []
meh (a:as) f = l2 (:) (f a) (meh as f)

flipType :: (Monad m) => [m a] -> m [a]
flipType ms = meh ms id
