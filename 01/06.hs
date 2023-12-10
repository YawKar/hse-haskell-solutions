sum'n'count :: Integer -> (Integer, Integer)
sum'n'count x
  | x < 0 = sum'n'count (abs x)
  | x < 10 = (x, 1)
  | otherwise =
      let (s', c') = sum'n'count (x `div` 10)
          (s, c) = sum'n'count (x `mod` 10)
       in (s + s', c + c')
