{-# OPTIONS_GHC -Wall #-}

drop' :: Int -> [a] -> [a]
drop' n xs = foldr step ini xs n

step :: a -> (Int -> [a]) -> Int -> [a]
step a g = f
  where
    f n
      | n <= 0 = a : g 0
      | otherwise = g (pred n)

ini :: Int -> [a]
ini _ = []
