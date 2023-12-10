data Tree a = Leaf | Node (Tree a) a (Tree a)
  deriving (Show)

instance Functor Tree where
  fmap _ Leaf = Leaf
  fmap f (Node l x r) = Node (f <$> l) (f x) (f <$> r)