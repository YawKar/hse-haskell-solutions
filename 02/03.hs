digits :: Integer -> [Integer]
digits n =
  let f n
        | n < 0 = f (-n)
        | n < 10 = [n]
        | otherwise = n `mod` 10 : f (n `div` 10)
   in reverse (f n)

containsAllDigits :: Integer -> Bool
containsAllDigits x = and (map (\d -> or (map (== d) (digits x))) [1 .. 9])
