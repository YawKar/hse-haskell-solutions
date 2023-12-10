data OddC a = Un a | Bi a a (OddC a) deriving (Eq, Show)

concat3OC :: OddC a -> OddC a -> OddC a -> OddC a
concat3OC (Un a0) (Un a1) x = Bi a0 a1 x
concat3OC (Un a0) (Bi a1 a2 x) y = Bi a0 a1 $ concat3OC (Un a2) x y
concat3OC (Bi a0 a1 x) y z = Bi a0 a1 $ concat3OC x y z

concatOC :: OddC (OddC a) -> OddC a
concatOC (Un x) = x
concatOC (Bi a0 a1 x) = concat3OC a0 a1 $ concatOC x

instance Functor OddC where
  fmap f (Un a) = Un $ f a
  fmap f (Bi a0 a1 x) = Bi (f a0) (f a1) $ fmap f x

instance Applicative OddC where
  pure = Un
  Un ab <*> x = ab <$> x
  Bi ab0 ab1 x <*> y = concat3OC (ab0 <$> y) (ab1 <$> y) (x <*> y)

instance Monad OddC where
  Un a >>= k = k a
  Bi a0 a1 x >>= k = concat3OC (k a0) (k a1) (x >>= k)
