module Traversables where

import           Data.Traversable
import           Test.QuickCheck          (Arbitrary, arbitrary, frequency)
import           Test.QuickCheck.Checkers
import           Test.QuickCheck.Classes

-- class (Functor t, Foldable t) =>
--       Traversable t
--   where
--
--   traverse :: Applicative f => (a -> f b) -> t a -> f (t b)
--   sequenceA :: Applicative f => t (f a) -> f (t a)
--   sequence :: Monad m => t (m a) -> m (t a)
--
-------- Traversable laws --------
--
-- Naturality --
-- t . traverse f = traverse (t . f)
-- t . sequenceA = sequenceA . fmap t
--
-- Identity --
-- traverse Identity = Identity
-- sequenceA . fmap Identity = Identity
--
-- Composition --
-- traverse (Compose . fmap g . f) = Compose . fmap (traverse g) . traverse f
-- sequenceA . fmap Compose = Compose . fmap sequenceA . sequenceA
--
newtype Identity a =
  Identity a
  deriving (Eq, Ord, Show)

instance Functor Identity where
  fmap f (Identity a) = Identity (f a)

instance Foldable Identity where
  foldMap f (Identity a) = f a

instance Traversable Identity where
  traverse f (Identity a) = Identity <$> f a

instance Arbitrary a => Arbitrary (Identity a) where
  arbitrary = Identity <$> arbitrary

instance Eq a => EqProp (Identity a) where
  (=-=) = eq

newtype Constant a b = Constant
  { getConstant :: a
  } deriving (Eq, Ord, Show)

instance Functor (Constant a) where
  fmap _ (Constant a) = Constant a

instance Foldable (Constant a) where
  foldr _ z _ = z

instance Traversable (Constant a) where
  traverse _ (Constant a) = pure $ Constant a

instance (Arbitrary a, Arbitrary b) => Arbitrary (Constant a b) where
  arbitrary = Constant <$> arbitrary

instance (Eq a, Eq b) => EqProp (Constant a b) where
  (=-=) = eq

data Optional a
  = Nada
  | Yep a
  deriving (Eq, Ord, Show)

instance Functor Optional where
  fmap _ Nada    = Nada
  fmap f (Yep a) = Yep $ f a

instance Foldable Optional where
  foldr _ z Nada    = z
  foldr f z (Yep a) = f a z

instance Traversable Optional where
  traverse _ Nada    = pure Nada
  traverse f (Yep a) = Yep <$> f a

instance Arbitrary a => Arbitrary (Optional a) where
  arbitrary = do
    a <- arbitrary
    frequency [(1, return Nada), (3, return (Yep a))]

instance Eq a => EqProp (Optional a) where
  (=-=) = eq

data List a
  = Nil
  | Cons a
         (List a)
  deriving (Eq, Ord, Show)

instance Functor List where
  fmap _ Nil        = Nil
  fmap f (Cons a l) = Cons (f a) (fmap f l)

instance Foldable List where
  foldr _ z Nil        = z
  foldr f z (Cons a l) = foldr f (f a z) l

instance Traversable List where
  traverse _ Nil        = pure Nil
  traverse f (Cons a l) = Cons <$> f a <*> traverse f l

instance Arbitrary a => Arbitrary (List a) where
  arbitrary = do
    a <- arbitrary
    frequency [(1, return Nil), (3, return (Cons a Nil))]

instance Eq a => EqProp (List a) where
  (=-=) = eq

data Three a b c =
  Three a
        b
        c
  deriving (Eq, Ord, Show)

instance Functor (Three a b) where
  fmap f (Three a b c) = Three a b (f c)

instance Foldable (Three a b) where
  foldr f z (Three a b c) = f c z

instance Traversable (Three a b) where
  traverse f (Three a b c) = Three a b <$> f c

instance (Arbitrary a, Arbitrary b, Arbitrary c) =>
         Arbitrary (Three a b c) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    c <- arbitrary
    return $ Three a b c

instance (Eq a, Eq b, Eq c) => EqProp (Three a b c) where
  (=-=) = eq

data Big a b =
  Big a
      b
      b
  deriving (Eq, Ord, Show)

instance Functor (Big a) where
  fmap f (Big a b b') = Big a (f b) (f b')

instance Foldable (Big a) where
  foldMap f (Big a b b') = mappend (f b) (f b')

instance Traversable (Big a) where
  traverse f (Big a b b') = Big a <$> f b <*> f b'

instance (Arbitrary a, Arbitrary b) => Arbitrary (Big a b) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    b' <- arbitrary
    return $ Big a b b'

instance (Eq a, Eq b) => EqProp (Big a b) where
  (=-=) = eq

data Tree a
  = Empty
  | Leaf a
  | Node (Tree a)
         a
         (Tree a)
  deriving (Eq, Show)

instance Functor Tree where
  fmap _ Empty         = Empty
  fmap f (Leaf a)      = Leaf (f a)
  fmap f (Node t a t') = Node (fmap f t) (f a) (fmap f t')

instance Foldable Tree where
  foldMap f Empty         = mempty
  foldMap f (Leaf a)      = f a
  foldMap f (Node t a t') = foldMap f t `mappend` f a `mappend` foldMap f t'

instance Traversable Tree where
  traverse f Empty         = pure Empty
  traverse f (Leaf a)      = Leaf <$> f a
  traverse f (Node t a t') = Node <$> traverse f t <*> f a <*> traverse f t'

instance Arbitrary a => Arbitrary (Tree a) where
  arbitrary = do
    a <- arbitrary
    a' <- arbitrary
    frequency
      [ (1, return Empty)
      , (2, return (Leaf a))
      , (4, return (Node (Leaf a) a' Empty))
      ]

instance Eq a => EqProp (Tree a) where
  (=-=) = eq
