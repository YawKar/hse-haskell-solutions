surround :: a -> a -> [a] -> [a]
surround x y zs = do
  z <- zs
  [x, z, y]
