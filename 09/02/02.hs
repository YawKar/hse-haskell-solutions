lookups :: (Eq k) => k -> [(k,v)] -> [v]
lookups x ys = do
  (yk, yv) <- ys
  [yv | x == yk]
