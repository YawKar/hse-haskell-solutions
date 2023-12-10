data Tree a = Leaf | Node (Tree a) a (Tree a)

instance (Show a) => Show (Tree a) where
  showsPrec d Leaf = showString "{}"
  showsPrec d (Node l x r) =
    showChar '<'
      . showsPrec d l
      . showString (show x)
      . showsPrec d r
      . showChar '>'
