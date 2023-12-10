{-# OPTIONS_GHC -Wall #-}

data Triple a = Tr a a a deriving (Eq, Show)

instance Functor Triple where
  -- fmap :: (a -> b) -> Triple a -> Triple b
  fmap f (Tr a b c) = Tr (f a) (f b) (f c)

instance Applicative Triple where
  -- pure :: a -> Triple a
  pure x = Tr x x x

  -- (<*>) :: Triple (a -> b) -> Triple a -> Triple b
  Tr f1 f2 f3 <*> Tr a b c = Tr (f1 a) (f2 b) (f3 c)

instance Foldable Triple where
  -- foldr :: (a -> b -> b) -> b -> Triple a -> b
  foldr f ini (Tr a b c) = a `f` (b `f` (c `f` ini))

instance Traversable Triple where
  -- traverse :: (Applicative f) => (a -> f b) -> Triple a -> f (Triple b)
  traverse f (Tr a b c) = Tr <$> f a <*> f b <*> f c
