{-# OPTIONS_GHC -Wall #-}

import Data.Function (on)
import Data.List (nub)

infixl 4 :@

infixr 3 :->

type Symb = String

-- Терм
data Expr
  = Var Symb
  | Expr :@ Expr
  | Lam Symb Expr
  deriving (Eq, Show)

-- Тип
data Type
  = TVar Symb
  | Type :-> Type
  deriving (Eq, Show)

-- Контекст
newtype Env = Env [(Symb, Type)]
  deriving (Eq, Show)

-- Подстановка
newtype SubsTy = SubsTy [(Symb, Type)]
  deriving (Eq, Show)

freeVars :: Expr -> [Symb]
freeVars = withBoundVars []
  where
    withBoundVars bounded (Var v) = [v | v `notElem` bounded]
    withBoundVars bounded (t1 :@ t2) = nub $ ((++) `on` withBoundVars bounded) t1 t2
    withBoundVars bounded (Lam bv t) = withBoundVars (bv : bounded) t

freeTVars :: Type -> [Symb]
freeTVars (s1 :-> s2) = nub $ ((++) `on` freeTVars) s1 s2
freeTVars (TVar s) = [s]

extendEnv :: Env -> Symb -> Type -> Env
extendEnv (Env env) s t = Env $ (s, t) : env

freeTVarsEnv :: Env -> [Symb]
freeTVarsEnv (Env env) = nub $ concatMap (freeTVars . snd) env
