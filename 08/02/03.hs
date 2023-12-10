{-# OPTIONS_GHC -Wall #-}

newtype Cmps f g x = Cmps {getCmps :: f (g x)} deriving (Eq, Show)

instance (Foldable f, Foldable g) => Foldable (Cmps f g) where
  foldr f ini (Cmps fga) = foldr (flip (foldr f)) ini fga
