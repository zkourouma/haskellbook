module Monoidal where

import           Data.Monoid
import           Test.QuickCheck

data Optional a
  = Nada
  | Only a
  deriving (Eq, Show)

instance Monoid a => Monoid (Optional a) where
  mempty = Nada
  mappend (Only x) Nada     = Only x
  mappend Nada (Only x)     = Only x
  mappend (Only x) (Only y) = Only (mappend x y)

-------- Monoid laws --------
--
-- Associativity --
monoidAssoc :: (Eq m, Monoid m) => m -> m -> m -> Bool
monoidAssoc a b c = (a <> (b <> c)) == ((a <> b) <> c)

-- Left Identity --
monoidLeftIdentity :: (Eq m, Monoid m) => m -> Bool
monoidLeftIdentity a = (mempty <> a) == a

-- Right Identity --
monoidRightIdentity :: (Eq m, Monoid m) => m -> Bool
monoidRightIdentity a = (a <> mempty) == a

newtype First' a = First'
  { getFirst' :: Optional a
  } deriving (Eq, Show)

instance Arbitrary a => Arbitrary (First' a) where
  arbitrary = do
    a <- arbitrary
    frequency [(1, return (First' Nada)), (3, return (First' (Only a)))]

instance Monoid a => Monoid (First' a) where
  mempty = First' Nada
  mappend (First' (Only x)) (First' Nada)     = First' (Only x)
  mappend (First' Nada) (First' (Only y))     = First' (Only y)
  mappend (First' (Only x)) (First' (Only y)) = First' (Only (mappend x y))

type FirstMappend = First' String -> First' String -> First' String -> Bool

type FstId = First' String -> Bool
