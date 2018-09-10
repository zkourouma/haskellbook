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

newtype Identity a =
  Identity a
  deriving (Eq, Show)

instance Monoid a => Monoid (Identity a) where
  mempty = Identity mempty
  mappend (Identity x) (Identity y) = Identity (mappend x y)

instance Arbitrary a => Arbitrary (Identity a) where
  arbitrary = Identity <$> arbitrary

type IdenMappend = Identity String -> Identity String -> Identity String -> Bool

type IdenId = Identity String -> Bool

data Two a b =
  Two a
      b
  deriving (Eq, Show)

instance (Monoid a, Monoid b) => Monoid (Two a b) where
  mempty = Two mempty mempty
  mappend (Two x y) (Two x' y') = Two (mappend x x') (mappend y y')

instance (Arbitrary a, Arbitrary b) => Arbitrary (Two a b) where
  arbitrary = do
    x <- arbitrary
    y <- arbitrary
    return $ Two x y

type TwoMappend
   = Two String String -> Two String String -> Two String String -> Bool

type TwoId = Two String String -> Bool

newtype BoolConj =
  BoolConj Bool
  deriving (Eq, Show)

instance Monoid BoolConj where
  mempty = BoolConj True
  mappend (BoolConj x) (BoolConj y) = BoolConj (x && y)

instance Arbitrary BoolConj where
  arbitrary = BoolConj <$> arbitrary

type BoolConjMappend = BoolConj -> BoolConj -> BoolConj -> Bool

type BoolConjId = BoolConj -> Bool

newtype BoolDisj =
  BoolDisj Bool
  deriving (Eq, Show)

instance Monoid BoolDisj where
  mempty = BoolDisj False
  mappend (BoolDisj x) (BoolDisj y) = BoolDisj (x || y)

instance Arbitrary BoolDisj where
  arbitrary = BoolDisj <$> arbitrary

type BoolDisjMappend = BoolDisj -> BoolDisj -> BoolDisj -> Bool

type BoolDisjId = BoolDisj -> Bool

newtype Combine a b = Combine
  { unCombine :: a -> b
  }

instance Monoid b => Monoid (Combine a b) where
  mempty = Combine (const mempty)
  mappend (Combine f) (Combine g) = Combine (\n -> mappend (f n) (g n))
