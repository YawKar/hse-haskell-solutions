{-# LANGUAGE InstanceSigs #-}
{-# OPTIONS_GHC -Wall #-}

import Data.Traversable (foldMapDefault)

data NEList a
  = Single a
  | Cons a (NEList a)
  deriving (Eq, Show)

instance Functor NEList where
  -- can't use `fmapDefault` from `Data.Traversable`
  -- on Traversable instances defined with only sequenceA
  -- bc it'll result in infinite recursion
  fmap :: (a -> b) -> NEList a -> NEList b
  fmap f (Single v) = Single $ f v
  fmap f (Cons a rest) = Cons (f a) $ f <$> rest

instance Foldable NEList where
  foldMap :: (Monoid m) => (a -> m) -> NEList a -> m
  foldMap = foldMapDefault

instance Traversable NEList where
  sequenceA :: (Applicative f) => NEList (f a) -> f (NEList a)
  sequenceA (Single v) = Single <$> v
  sequenceA (Cons a rest) = Cons <$> a <*> sequenceA rest
