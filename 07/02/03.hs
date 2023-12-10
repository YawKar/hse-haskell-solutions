{-# OPTIONS_GHC -Wall #-}

data Tree a = Nil | Branch (Tree a) a (Tree a) deriving (Eq, Show)

instance Functor Tree where
  fmap _ Nil = Nil
  fmap f (Branch l x r) = Branch (f <$> l) (f x) (f <$> r)

instance Applicative Tree where
  pure x = Branch (pure x) x (pure x)

  Branch lf f rf <*> Branch l x r = Branch (lf <*> l) (f x) (rf <*> r)
  _ <*> _ = Nil
