import Data.Function (on)
import Data.List (nub)

type Symb = String

infixl 2 :@

data Expr
  = Var Symb
  | Expr :@ Expr
  | Lam Symb Expr
  deriving (Show, Eq)

omegaCombinator :: Expr
omegaCombinator = Lam "x" $ Var "x" :@ Var "x"

subst :: Symb -> Expr -> Expr -> Expr
subst var newT = withNTFrees [] []
  where
    nTFrees = freeVars newT
    withNTFrees :: [(Symb, Symb)] -> [Symb] -> Expr -> Expr
    withNTFrees renamings bounded mT
      | Var mVar <- mT = maybe (if mVar `notElem` bounded && mVar == var then newT else Var mVar) Var (lookup mVar renamings)
      | t1 :@ t2 <- mT = ((:@) `on` withNTFrees renamings bounded) t1 t2
      | Lam bv t <- mT =
          let newBv = bv ++ "'"
           in if bv `elem` nTFrees
                then Lam newBv $ withNTFrees ((bv, newBv) : renamings) (newBv : bounded) t
                else Lam bv $ withNTFrees renamings (bv : bounded) t

freeVars :: Expr -> [Symb]
freeVars = withBoundVars []
  where
    withBoundVars bounded (Var v) = [v | v `notElem` bounded]
    withBoundVars bounded (t1 :@ t2) = nub $ ((++) `on` withBoundVars bounded) t1 t2
    withBoundVars bounded (Lam bv t) = withBoundVars (bv : bounded) t
