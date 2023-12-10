digits :: Integer -> [Integer]
digits n =
  let f n
        | n < 0 = f (-n)
        | n < 10 = [n]
        | otherwise = n `mod` 10 : f (n `div` 10)
   in reverse (f n)

-- containsAllDigitsOnes :: Integer -> Bool
-- containsAllDigitsOnes n = sort (filter (/= 0) (digits n)) == [1 .. 9]

containsAllDigitsOnes :: Integer -> Bool
containsAllDigitsOnes x = and (map (\d -> length (filter (== d) (digits x)) == 1) [1 .. 9])
