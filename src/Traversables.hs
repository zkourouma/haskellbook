module Traversables where
-- class (Functor t, Foldable t) =>
--       Traversable t
--   where
--
--   traverse :: Applicative f => (a -> f b) -> t a -> f (t b)
--   traverse f = sequenceA . fmap f
--
--   sequenceA :: Applicative f => t (f a) -> f (t a)
--   sequenceA = traverse id
