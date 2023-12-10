import Data.List (intercalate)

newtype Matrix a = Matrix [[a]]

instance (Show a) => Show (Matrix a) where
  show (Matrix mat)
    | null mat = "EMPTY"
    | otherwise = intercalate "\n" $ map show mat
