-- digits :: Integer -> [Integer]
-- digits n =
--   let f n
--         | n < 0 = f (-n)
--         | n < 10 = [n]
--         | otherwise = n `mod` 10 : f (n `div` 10)
--    in reverse (f n)

digits :: Integer -> [Integer]
digits n =
  if n < 0 then
    digits (-n)
  else if n < 10 then
    [n]
  else
    digits (n `div` 10) ++ [n `mod` 10]

