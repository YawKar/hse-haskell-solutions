{-# OPTIONS_GHC -Wall #-}

tails' :: [a] -> [[a]]
tails' = foldr fun ini

fun :: a -> [[a]] -> [[a]]
fun x tailss = (x : head tailss) : tailss

ini :: [[a]]
ini = [[]]

inits' :: [a] -> [[a]]
inits' = foldr fun' ini'

fun' :: a -> [[a]] -> [[a]]
fun' x initss = [] : [x : init' | init' <- initss]

ini' :: [[a]]
ini' = [[]]
