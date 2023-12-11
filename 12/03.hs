{-# OPTIONS_GHC -Wall #-}

import Data.Function (on)
import Data.List (elemIndex)

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

appSubsTy :: SubsTy -> Type -> Type
appSubsTy (SubsTy sst) t@(TVar s)
  | Just i <- s `elemIndex` map fst sst = snd $ sst !! i
  | otherwise = t
appSubsTy sst (t1 :-> t2) = ((:->) `on` appSubsTy sst) t1 t2

appSubsEnv :: SubsTy -> Env -> Env
appSubsEnv = undefined
