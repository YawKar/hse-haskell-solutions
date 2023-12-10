absDiff :: Num a => [a] -> [a]
absDiff (a : t@(b : xs)) = do
  abs (a - b) : absDiff t
absDiff _ = []
