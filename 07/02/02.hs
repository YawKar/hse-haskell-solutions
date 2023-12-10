data Triple a = Tr a a a deriving (Eq, Show)

instance Functor Triple where
  fmap f (Tr a b c) = Tr (f a) (f b) (f c)

instance Applicative Triple where
  pure x = Tr x x x

  Tr f1 f2 f3 <*> Tr a b c = Tr (f1 a) (f2 b) (f3 c)
