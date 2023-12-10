import Data.List (tails)

comb :: Int -> [a] -> [[a]]
comb 0 _ = [[]]
comb _ [] = []
comb n xs = [head xs' : subcomb | xs' <- tails xs, not . null $ xs', subcomb <- comb (pred n) (tail xs')]
