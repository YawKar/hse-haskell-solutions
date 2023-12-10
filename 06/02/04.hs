{-# OPTIONS_GHC -Wall #-}

(!!!) :: [a] -> Int -> Maybe a
xs !!! n = foldr fun ini xs n

fun :: a -> (Int -> Maybe a) -> (Int -> Maybe a)
fun a fini index
  | index < 0 = Nothing
  | index == 0 = Just a
  | otherwise = fini (pred index)

ini :: Int -> Maybe a
ini _ = Nothing
