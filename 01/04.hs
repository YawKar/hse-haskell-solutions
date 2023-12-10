seqB :: Integer -> Integer
seqB n
  | n == 0 = 1
  | n == 1 = 2
  | True = r 2 n 1 2 3

r i n b0 b1 b3
  | i == n = b3
  | True = r (i + 1) n b1 b3 (b3 - 2 * b1 + 3 * b0)
