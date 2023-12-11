{-# LANGUAGE FlexibleContexts #-}
{-# OPTIONS_GHC -Wall #-}

import Control.Monad.Except (MonadError (throwError))
import Data.Function (on)
import Data.List (elemIndex, nub)

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

freeTVars :: Type -> [Symb]
freeTVars (s1 :-> s2) = nub $ ((++) `on` freeTVars) s1 s2
freeTVars (TVar s) = [s]

instance Semigroup SubsTy where
  (<>) = composeSubsTy

instance Monoid SubsTy where
  mempty = SubsTy []

appSubsTy :: SubsTy -> Type -> Type
appSubsTy (SubsTy sst) t@(TVar s)
  | Just i <- s `elemIndex` map fst sst = snd $ sst !! i
  | otherwise = t
appSubsTy sst (t1 :-> t2) = ((:->) `on` appSubsTy sst) t1 t2

composeSubsTy :: SubsTy -> SubsTy -> SubsTy
composeSubsTy sst1'@(SubsTy sst1) (SubsTy sst2) = SubsTy $ nub $ [(s, appSubsTy sst1' t) | (s, t) <- sst2] <> sst1

unify :: (MonadError String m) => Type -> Type -> m SubsTy
unify (TVar leftName) right
  | leftName `notElem` freeTVars right = return $ SubsTy [(leftName, right)]
unify left (TVar rightName)
  | rightName `notElem` freeTVars left = return $ SubsTy [(rightName, left)]
unify (leftFrom :-> leftTo) (rightFrom :-> rightTo) = do
  left <- unify leftFrom rightFrom
  right <- unify leftTo rightTo
  return $ left <> right
unify a b = throwError $ "Can't unify (" <> show a <> ") with (" <> show b <> ")!"
