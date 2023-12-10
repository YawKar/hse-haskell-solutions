data Tree a = Leaf | Node (Tree a) a (Tree a)
  deriving (Show)

instance (Eq a) => Eq (Tree a) where
  (/=) Leaf Leaf = False
  (/=) (Node l0 x0 r0) (Node l1 x1 r1) = x0 /= x1 || bfsNeq [(l0, l1), (r0, r1)]
    where
      bfsNeq [] = False
      bfsNeq pairs = anyNeq || bfsNeq (concatMap pushDown pairs)
        where
          anyNeq = any neq pairs
          neq (Leaf, Leaf) = False
          neq (Node _ x0 _, Node _ x1 _) = x0 /= x1
          pushDown (Leaf, Leaf) = []
          pushDown (Node l0 _ r0, Node l1 _ r1) = [(l0, l1), (r0, r1)]
