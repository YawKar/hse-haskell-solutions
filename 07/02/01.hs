import Control.Applicative (ZipList (ZipList), getZipList)

infixl 1 >$<

(>$<) :: (a -> b) -> [a] -> [b]
f >$< as = map f as

infixl 0 >*<

(>*<) :: [a -> b] -> [a] -> [b]
funcs >*< as = zipWith id funcs as
