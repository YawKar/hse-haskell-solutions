data B a = Empty | Zero a | One a deriving (Eq, Show)

instance Functor B where
  fmap _ Empty = Empty
  fmap f (Zero v) = Zero (f v)
  fmap f (One v) = One (f v)

type Bin = Fix B

phiB :: B Int -> Int
phiB Empty = 0
phiB (Zero v) = v * 2
phiB (One v) = v * 2 + 1

bin2int :: Bin -> Int
bin2int = cata phiB

psiB :: Int -> B Int
psiB 0 = Empty
psiB n
  | even n = Zero (n `div` 2)
  | otherwise = One (n `div` 2)

int2bin :: Int -> Bin
int2bin = ana psiB
