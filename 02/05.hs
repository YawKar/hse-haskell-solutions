-- sublist :: Int -> Int -> [a] -> [a]
-- sublist n k xs
--   | n < 0 = sublist 0 k xs
--   | k < 0 = sublist n 0 xs
--   | otherwise =
--       let length = k - n
--        in take length (drop n xs)

sublist :: Int -> Int -> [a] -> [a]
sublist n k list =
  if k < 0 then
    sublist n 0 list
  else if n < 0 then
    sublist 0 k list
  else
    take (k - n) (drop n list)
