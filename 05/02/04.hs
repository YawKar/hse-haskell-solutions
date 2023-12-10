rotate :: Int -> [a] -> [a]
rotate _ [] = []
rotate n xs
  | n >= 0 = helper1 n xs []
  | otherwise = helper1 n' xs []
  where
    n' = (len - (abs n `mod` len)) `mod` len
    len = fromIntegral . length $ xs

helper1 :: Int -> [a] -> [a] -> [a]
helper1 0 xs acc = xs ++ reverse acc
helper1 n xs acc
  -- met the end
  | null xs = helper2 n (reverse acc)
  | otherwise = helper1 (pred n) (tail xs) (head xs : acc)

helper2 :: Int -> [a] -> [a]
helper2 _ [] = []
helper2 n xs = suff ++ pref
  where
    (pref, suff) = splitAt n' xs
    len = fromIntegral . length $ xs
    n' = n `mod` len
