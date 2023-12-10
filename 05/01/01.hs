data Tree a = Leaf | Node (Tree a) a (Tree a)
  deriving (Show, Eq)

elemTree :: (Eq a) => a -> Tree a -> Bool
elemTree v = bfs . pure
  where
    bfs [] = False
    bfs roots = anyLeaf || bfs (concatMap (\(Node l _ r) -> [l, r]) $ filter (/= Leaf) roots)
      where
        anyLeaf = any ((Just v ==) . toMaybeVal) roots
        toMaybeVal Leaf = Nothing
        toMaybeVal (Node _ x _) = Just x
