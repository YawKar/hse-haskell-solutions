-- movingLists :: Int -> [a] -> [[a]]
-- movingLists _ [] = []
-- movingLists n xs =
--   let list = take n xs
--    in if length list == n
--         then list : movingLists n (tail xs)
--         else []

movingLists :: Int -> [a] -> [[a]]
movingLists n xs =
  if null xs then
    []
  else if length (take n xs) == n then
    take n xs : movingLists n (tail xs)
  else
    []
