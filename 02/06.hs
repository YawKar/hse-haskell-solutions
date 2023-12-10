-- repeatEveryElem :: Int -> [a] -> [a]
-- repeatEveryElem _ [] = []
-- repeatEveryElem k (x : xs) =
--   let build _ 0 = []
--       build x k = x : build x (k - 1)
--    in build x k ++ repeatEveryElem k xs

repeatEveryElem :: Int -> [a] -> [a]
repeatEveryElem k list =
  if null list then
    []
  else
    replicate k (head list) ++ repeatEveryElem k (tail list)
