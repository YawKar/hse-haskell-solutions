{-# LANGUAGE InstanceSigs #-}
{-# OPTIONS_GHC -Wall #-}

import Data.Traversable (fmapDefault, foldMapDefault)

data Result a
  = Ok a
  | Error String
  deriving (Eq, Show)

instance Functor Result where
  fmap :: (a -> b) -> Result a -> Result b
  fmap = fmapDefault

instance Foldable Result where
  foldMap :: (Monoid m) => (a -> m) -> Result a -> m
  foldMap = foldMapDefault

instance Traversable Result where
  traverse :: (Applicative f) => (a -> f b) -> Result a -> f (Result b)
  traverse _ (Error e) = pure $ Error e
  traverse f (Ok a) = Ok <$> f a
