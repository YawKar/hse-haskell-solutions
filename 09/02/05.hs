data OddC a = Un a | Bi a a (OddC a) deriving (Eq,Show)

concat3OC :: OddC a -> OddC a -> OddC a -> OddC a
concat3OC (Un a0) (Un a1) x = Bi a0 a1 x
concat3OC (Un a0) (Bi a1 a2 x) y = Bi a0 a1 $ concat3OC (Un a2) x y
concat3OC (Bi a0 a1 x) y z = Bi a0 a1 $ concat3OC x y z
