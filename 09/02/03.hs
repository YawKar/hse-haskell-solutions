factor2 :: Integer -> [(Integer, Integer)]
factor2 n = do
  a <- [1 .. round (sqrt $ fromIntegral n)]
  [(a, n `div` a) | n `mod` a == 0]
