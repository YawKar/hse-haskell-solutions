{-# OPTIONS_GHC -Wall #-}

data Tree a = Nil | Branch (Tree a) a (Tree a) deriving (Eq, Show)

instance Functor Tree where
  fmap _ Nil = Nil
  fmap f (Branch l x r) = Branch (f <$> l) (f x) (f <$> r)

instance Applicative Tree where
  pure x = Branch (pure x) x (pure x)

  Branch lf f rf <*> Branch l x r = Branch (lf <*> l) (f x) (rf <*> r)
  _ <*> _ = Nil

instance Foldable Tree where
  foldr _ ini Nil = ini
  foldr f ini (Branch left x right) = foldr f (x `f` foldr f ini right) left

instance Traversable Tree where
  traverse _ Nil = pure Nil
  traverse f (Branch left a right) = Branch <$> traverse f left <*> f a <*> traverse f right
