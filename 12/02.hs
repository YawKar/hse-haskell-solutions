{-# LANGUAGE FlexibleContexts #-}

import Control.Monad.Except
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

appEnv :: (MonadError String m) => Env -> Symb -> m Type
appEnv (Env xs) v
  | Just i <- v `elemIndex` map fst xs = pure $ snd $ xs !! i
  | otherwise = throwError $ "There is no variable \"" <> v <> "\" in the environment."
