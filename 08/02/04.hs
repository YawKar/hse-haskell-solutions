{-# LANGUAGE FlexibleContexts #-}
{-# OPTIONS_GHC -Wall #-}

newtype Cmps f g x = Cmps {getCmps :: f (g x)} deriving (Eq, Show)

instance (Foldable f, Foldable g) => Foldable (Cmps f g) where
  foldr f ini (Cmps fga) = foldr (flip (foldr f)) ini fga

instance (Functor f, Functor g) => Functor (Cmps f g) where
  fmap f (Cmps c) = Cmps $ (f <$>) <$> c

instance (Applicative f, Applicative g) => Applicative (Cmps f g) where
  pure = Cmps . pure . pure

  Cmps fgf <*> Cmps fgx = Cmps $ (<*>) <$> fgf <*> fgx

instance (Traversable f, Traversable g) => Traversable (Cmps f g) where
  traverse f (Cmps c) = Cmps <$> traverse (traverse f) c
