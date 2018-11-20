module Foldables where

--
-- foldr :: Foldable t => (a -> b -> b) -> b -> t a -> b
-- foldl :: Foldable t => (b -> a -> b) -> b -> t a -> b
-- foldMap :: (Monoid m, Foldable t) => (a -> m) -> t a -> m
--
newtype Identity a =
  Identity a
  deriving (Eq, Show)

instance Foldable Identity where
  foldr f z (Identity x) = f x z
  foldl f z (Identity x) = f z x
  foldMap f (Identity x) = f x

sum :: (Foldable t, Num a) => t a -> a
sum = foldr (+) 0

product :: (Foldable t, Num a) => t a -> a
product = foldr (*) 1

elem :: (Foldable t, Eq a) => a -> t a -> Bool
elem x = foldr (\y isElem -> y == x || isElem) False

comparison :: (Ord a) => (a -> a -> Bool) -> a -> Maybe a -> Maybe a
comparison _ x Nothing = Just x
comparison c x (Just y) =
  if c x y
    then Just x
    else Just y

minimum :: (Foldable t, Ord a) => t a -> Maybe a
minimum = foldr (comparison (<)) Nothing

maximum :: (Foldable t, Ord a) => t a -> Maybe a
maximum = foldr (comparison (>)) Nothing

null :: (Foldable t) => t a -> Bool
null = foldr (\_ _ -> False) True

length :: (Foldable t) => t a -> Int
length = foldr (\_ l -> l + 1) 0

toList :: (Foldable t) => t a -> [a]
toList = foldMap (: [])

fold :: (Foldable t, Monoid m) => t m -> m
fold = foldMap (mappend mempty)

foldMap' :: (Foldable t, Monoid m) => (a -> m) -> t a -> m
foldMap' f = foldr (mappend . f) mempty

newtype Constant a b =
  Constant b
  deriving (Eq, Show)

instance Foldable (Constant a) where
  foldr f z (Constant b) = f b z
  foldl f z (Constant b) = f z b
  foldMap f (Constant b) = f b

data Two a b =
  Two a
      b
  deriving (Eq, Show)

instance Foldable (Two a) where
  foldr f z (Two _ b) = f b z
  foldl f z (Two _ b) = f z b
  foldMap f (Two _ b) = f b

data Three a b c =
  Three a
        b
        c
  deriving (Eq, Show)

instance Foldable (Three a b) where
  foldr f z (Three _ _ c) = f c z
  foldl f z (Three _ _ c) = f z c
  foldMap f (Three _ _ c) = f c

data Three' a b =
  Three' a
         b
         b
  deriving (Eq, Show)

instance Foldable (Three' a) where
  foldr f z (Three' _ b b') = f b (f b' z)
  foldl f z (Three' _ b b') = f (f z b') b
  foldMap f (Three' _ b b') = mappend (f b) (f b')

filterF ::
     (Applicative f, Foldable t, Monoid (f a)) => (a -> Bool) -> t a -> f a
filterF f =
  foldMap
    (\x ->
       if f x
         then pure x
         else mempty)
