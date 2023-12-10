fibonacci :: Integer -> Integer
fibonacci n
  | n > 1 = r 1 n 0 1
  | n < 0 =
      let res = r 1 (-n) 0 1
       in if even n
            then (-res)
            else res
  | otherwise = n

r :: Integer -> Integer -> Integer -> Integer -> Integer
r i n f0 f1
  | i == n = f1
  | otherwise = r (i + 1) n f1 (f0 + f1)
