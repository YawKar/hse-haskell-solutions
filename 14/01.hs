import Data.Foldable (traverse_)

globalFunc n m
  | k == 1 = "1"
  | k == 2 = "2"
  where
    k = n + 1

kek = 1 `globalFunc` 4

data Jek

instance Bounded Jek where
  minBound :: Jek
  minBound = undefined
  maxBound :: Jek
  maxBound = undefined

-- instance Enum Jek where
--   toEnum :: Int -> Jek
--   toEnum = undefined
--   fromEnum :: Jek -> Int
--   fromEnum = undefined

main :: IO ()
main = do
  traverse_ putStrLn $ map show [y | y <- [x < 0 | x <- [0 ..]]]

newtype NJek = NJek Jek

lol :: NJek -> NJek
lol j = case j of
  NJek{} -> undefined
  x -> undefined

data Bar = Bar { foo :: Integer }

bar Bar{foo = bas} = foo
bar' Bar{foo = bas} = bas
