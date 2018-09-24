{-# LANGUAGE FlexibleInstances #-}

module Functorial where

import           Test.QuickCheck
import           Test.QuickCheck.Function

{-# ANN functorIdentity "HLint: ignore" #-}

{-# ANN functorCompose "HLint: ignore" #-}

{-# ANN functorCompose' "HLint: ignore" #-}

------ Functor Laws ------
-- Identity --
-- fmap id == id
-- Composition --
-- fmap (f . g) == fmap f . fmap g
-- Structure Preservation --
functorIdentity :: (Functor f, Eq (f a)) => f a -> Bool
functorIdentity f = fmap id f == f

functorCompose :: (Eq (f c), Functor f) => (a -> b) -> (b -> c) -> f a -> Bool
functorCompose f g x = (fmap g (fmap f x)) == (fmap (g . f) x)

functorCompose' :: (Eq (f c), Functor f) => f a -> Fun a b -> Fun b c -> Bool
functorCompose' x (Fun _ f) (Fun _ g) =
  (fmap (g . f) x) == (fmap g . fmap f $ x)

newtype Identity a =
  Identity a
  deriving (Eq, Show)

instance Functor Identity where
  fmap f (Identity a) = Identity (f a)

instance Arbitrary a => Arbitrary (Identity a) where
  arbitrary = Identity <$> arbitrary

type IdId = Identity Int -> Bool

type IntToInt = Fun Int Int

type IdComp = Identity Int -> IntToInt -> IntToInt -> Bool

data Pair a =
  Pair a
       a
  deriving (Eq, Show)

instance Functor Pair where
  fmap f (Pair a b) = Pair (f a) (f b)

instance Arbitrary a => Arbitrary (Pair a) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    return $ Pair a b

type PairId = Pair Int -> Bool

type PairComp = Pair Int -> IntToInt -> IntToInt -> Bool

data Two a b =
  Two a
      b
  deriving (Eq, Show)

instance Functor (Two a) where
  fmap f (Two a b) = Two a (f b)

instance (Arbitrary a, Arbitrary b) => Arbitrary (Two a b) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    return $ Two a b

type TwoId = Two Int Int -> Bool

type TwoComp = Two Int Int -> IntToInt -> IntToInt -> Bool

data Three a b c =
  Three a
        b
        c
  deriving (Eq, Show)

instance Functor (Three a b) where
  fmap f (Three a b c) = Three a b (f c)

instance (Arbitrary a, Arbitrary b, Arbitrary c) =>
         Arbitrary (Three a b c) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    c <- arbitrary
    return $ Three a b c

type ThreeId = Three Int Int Int -> Bool

type ThreeComp = Three Int Int Int -> IntToInt -> IntToInt -> Bool

data Three' a b =
  Three' a
         b
         b
  deriving (Eq, Show)

instance Functor (Three' a) where
  fmap f (Three' a b c) = Three' a (f b) (f c)

instance (Arbitrary a, Arbitrary b) => Arbitrary (Three' a b) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    c <- arbitrary
    return $ Three' a b c

type Three'Id = Three' Int Int -> Bool

type ThreePrimeComp = Three' Int Int -> IntToInt -> IntToInt -> Bool

data Four a b c d =
  Four a
       b
       c
       d
  deriving (Eq, Show)

instance Functor (Four a b c) where
  fmap f (Four a b c d) = Four a b c (f d)

instance (Arbitrary a, Arbitrary b, Arbitrary c, Arbitrary d) =>
         Arbitrary (Four a b c d) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    c <- arbitrary
    d <- arbitrary
    return $ Four a b c d

type FourId = Four Int Int Int Int -> Bool

type FourComp = Four Int Int Int Int -> IntToInt -> IntToInt -> Bool

data Four' a b =
  Four' a
        a
        a
        b
  deriving (Eq, Show)

instance Functor (Four' a) where
  fmap f (Four' a b c d) = Four' a b c (f d)

instance (Arbitrary a, Arbitrary b) => Arbitrary (Four' a b) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    c <- arbitrary
    d <- arbitrary
    return $ Four' a b c d

type Four'Id = Four' Int Int -> Bool

type FourPrimeComp = Four' Int Int -> IntToInt -> IntToInt -> Bool

data Quant a b
  = Finance
  | Desk a
  | Bloor b

instance Functor (Quant a) where
  fmap _ Finance   = Finance
  fmap _ (Desk a)  = Desk a
  fmap f (Bloor b) = Bloor (f b)

newtype K a b =
  K a

instance Functor (K a) where
  fmap _ (K a) = K a

newtype Flip f a b =
  Flip (f b a)
  deriving (Eq, Show)

instance Functor (Flip K a) where
  fmap f (Flip (K a)) = Flip (K (f a))

newtype EvilGoateeConst a b =
  GoatyConst b

instance Functor (EvilGoateeConst a) where
  fmap f (GoatyConst b) = GoatyConst (f b)

newtype LiftItOut f a =
  LiftItOut (f a)

instance Functor f => Functor (LiftItOut f) where
  fmap f (LiftItOut g) = LiftItOut (fmap f g)

data Parappa f g a =
  DaWrappa (f a)
           (g a)

instance (Functor f, Functor g) => Functor (Parappa f g) where
  fmap f (DaWrappa f' g) = DaWrappa (fmap f f') (fmap f g)

data IgnoreOne f g a b =
  IgnoreSomething (f a)
                  (g b)

instance Functor g => Functor (IgnoreOne f g a) where
  fmap f (IgnoreSomething f' g) = IgnoreSomething f' (fmap f g)

data Notorious g o a t =
  Notorious (g o)
            (g a)
            (g t)

instance Functor g => Functor (Notorious g o a) where
  fmap f (Notorious o a t) = Notorious o a (fmap f t)

data List a
  = Nil
  | Cons a
         (List a)

instance Functor List where
  fmap _ Nil         = Nil
  fmap f (Cons a as) = Cons (f a) (fmap f as)

data GoatLord a
  = NoGoat
  | OneGoat a
  | MoreGoats (GoatLord a)
              (GoatLord a)
              (GoatLord a)

instance Functor GoatLord where
  fmap _ NoGoat            = NoGoat
  fmap f (OneGoat a)       = OneGoat (f a)
  fmap f (MoreGoats a b c) = MoreGoats (fmap f a) (fmap f b) (fmap f c)

data TalkToMe a
  = Halt
  | Print String
          a
  | Read (String -> a)

instance Functor TalkToMe where
  fmap _ Halt        = Halt
  fmap f (Print s a) = Print s (f a)
  fmap f (Read g)    = Read (f . g)
