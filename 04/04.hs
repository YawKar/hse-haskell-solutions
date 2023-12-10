import Control.Applicative ((<|>))
import Data.Function (on)
import Data.List (nub)

type Symb = String

infixl 2 :@

data Expr
  = Var Symb
  | Expr :@ Expr
  | Lam Symb Expr
  deriving (Show, Read, Eq)

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

infix 1 `alphaEq`

alphaEq :: Expr -> Expr -> Bool
alphaEq e1 e2 = alphaEq' e1 (freeVars e1) e2 (freeVars e2) 0

alphaEq' :: Expr -> [Symb] -> Expr -> [Symb] -> Int -> Bool
alphaEq' (Var v0) fv0 (Var v1) fv1 _
  | (v0 `elem` fv0) /= (v1 `elem` fv1) = False
  | otherwise = v0 == v1
alphaEq' (t00 :@ t01) fv0 (t10 :@ t11) fv1 bvRen =
  alphaEq' t00 fv0 t10 fv1 bvRen && alphaEq' t01 fv0 t11 fv1 bvRen
alphaEq' (Lam bv0 t0) fv0 (Lam bv1 t1) fv1 bvRen =
  alphaEq' t0' fv0 t1' fv1 (succ bvRen)
  where
    commonBv = Var $ "_" ++ show bvRen
    t0' = subst bv0 commonBv t0
    t1' = subst bv1 commonBv t1
alphaEq' _ _ _ _ _ = False

reduceOnce :: Expr -> Maybe Expr
reduceOnce (Lam bv t) = (Lam bv <$>) $ reduceOnce t
reduceOnce (Lam bv lamt :@ nechto) = Just $ subst bv nechto lamt
reduceOnce (t0 :@ t1) = ((:@ t1) <$> reduceOnce t0) <|> ((t0 :@) <$> reduceOnce t1)
reduceOnce _ = Nothing

nf :: Expr -> Expr
nf term = maybe term nf $ reduceOnce term