doubleFact :: Integer -> Integer
doubleFact n
  | n <= 2 = n
  | otherwise = n * doubleFact (n - 2)
