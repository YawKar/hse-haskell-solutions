data T a x = Leaf | Branch x a x deriving (Show, Eq) -- deriving Eq для целей тестирования, не убирайте

instance Functor (T a) where
  fmap _ Leaf = Leaf
  fmap f (Branch l x r) = Branch (f l) x (f r)

type Tree a = Fix (T a)

phiTInorder :: Algebra (T a) [a] -- T a [a] -> [a]
phiTInorder Leaf = []
phiTInorder (Branch l x r) = l ++ [x] ++ r

tree2listInorder :: Tree a -> [a]
tree2listInorder = cata phiTInorder

psiTBST :: (Ord a) => Coalgebra (T a) [a] -- [a] -> T a [a]
psiTBST [] = Leaf
psiTBST (x : xs) = Branch (filter (<= x) xs) x (filter (> x) xs)

list2BST :: (Ord a) => [a] -> Tree a
list2BST = ana psiTBST

sort :: (Ord a) => [a] -> [a]
sort = hylo phiTInorder psiTBST
