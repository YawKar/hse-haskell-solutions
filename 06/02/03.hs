{-# OPTIONS_GHC -Wall #-}

reverse' :: [a] -> [a]
reverse' = foldr fun' ini'

fun' a = (++ [a])

ini' = []

reverse'' :: [a] -> [a]
reverse'' = foldl fun'' ini''

fun'' = flip (:)

ini'' = []
