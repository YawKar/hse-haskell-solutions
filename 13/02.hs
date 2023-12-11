data E e
  = Num Int
  | Add e e
  | Mult e e

instance Functor E where
  fmap f (Num e) = Num e
  fmap f (Add e1 e2) = Add (f e1) (f e2)
  fmap f (Mult e1 e2) = Mult (f e1) (f e2)

type Expr = Fix E

phiE :: E Int -> Int
phiE (Num v) = v
phiE (Add e1 e2) = e1 + e2
phiE (Mult e1 e2) = e1 * e2

eval :: Expr -> Int
eval = cata phiE

phiEShow :: E String -> String
phiEShow (Num v) = show v
phiEShow (Add e1 e2) = "(" <> e1 <> "+" <> e2 <> ")"
phiEShow (Mult e1 e2) = "(" <> e1 <> "*" <> e2 <> ")"
