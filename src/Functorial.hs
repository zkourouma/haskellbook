module Functorial where

{-# ANN functorIdentity "HLint: ignore" #-}

{-# ANN functorCompose "HLint: ignore" #-}

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
