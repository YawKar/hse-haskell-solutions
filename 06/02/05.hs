{-# OPTIONS_GHC -Wall #-}

foldl'' :: (b -> a -> b) -> b -> [a] -> b
foldl'' f v xs = foldr (fun f) ini xs v

fun :: (b -> a -> b) -> a -> (b -> b) -> (b -> b)
fun fl a resf b = resf $ fl b a

ini :: a -> a
ini = id
