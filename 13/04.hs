data T a x = Leaf | Branch x a x

instance Functor (T a) where
  fmap _ Leaf = Leaf
  fmap f (Branch l x r) = Branch (f l) x (f r)

type Tree a = Fix (T a)

phiTSum :: Algebra (T Integer) Integer
phiTSum Leaf = 0
phiTSum (Branch l x r) = l + x + r

treeSum :: Tree Integer -> Integer
treeSum = cata phiTSum
