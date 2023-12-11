data E e
  = Num Int
  | Add e e
  | Mult e e

instance Functor E where
  fmap f (Num e) = Num e
  fmap f (Add e1 e2) = Add (f e1) (f e2)
  fmap f (Mult e1 e2) = Mult (f e1) (f e2)

type Expr = Fix E

phiEShowS :: E ShowS -> ShowS
phiEShowS (Num f) = (show f <>)
phiEShowS (Add f1 f2) = ("+ " <>) . f1 . (" " <>) . f2
phiEShowS (Mult f1 f2) = ("* " <>) . f1 . (" " <>) . f2

type Stack = [Int]

push :: Int -> Stack -> Stack
push a as = a : as

add :: Stack -> Stack
add (a : b : cs) = (b + a) : cs

mult :: Stack -> Stack
mult (a : b : cs) = (b * a) : cs

phiE' :: E (Stack -> Stack) -> Stack -> Stack
phiE' (Num v) = push v
phiE' (Add f1 f2) = add . f1 . f2
phiE' (Mult f1 f2) = mult . f1 . f2

eval' :: Expr -> Stack -> Stack
eval' = cata phiE'
