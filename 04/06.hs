{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE TupleSections #-}
{-# OPTIONS_GHC -Wall #-}

import Control.Applicative ((<|>))
import Data.List (foldl1')
import GHC.Show (showSpace)
import Text.Parsec (Parsec, alphaNum, between, char, many, many1, parse, sepEndBy1, space, string, try)

type Symb = String

infixl 2 :@

data Expr
  = Var Symb
  | Expr :@ Expr
  | Lam Symb Expr
  deriving (Eq)

instance Show Expr where
  showsPrec :: Int -> Expr -> ShowS
  showsPrec _ (Var v) = showString v
  showsPrec d (Lam bv t) =
    showParen (d > 10) $
      showChar '\\'
        . showString bv
        . showSpace
        . showString "->"
        . showSpace
        . shows t
  showsPrec d (t0 :@ t1) =
    showParen (d > 10) $
      showsPrec 11 t0
        . showSpace
        . showsPrec 11 t1

instance Read Expr where
  readsPrec :: Int -> ReadS Expr
  readsPrec _ str = either (const []) (pure . (,"")) (parse parseExpr "f" str)

parseExpr :: Parsec String () Expr
parseExpr =
  parseLambda
    <|> try parseApp
    <|> parseVar

parseLambda :: Parsec String () Expr
parseLambda = do
  _ <- char '\\'
  boundedVars <- parseVarLit `sepEndBy1` many1 space
  _ <- string "->"
  _ <- many space
  body <- parseExpr
  return $ foldr Lam body boundedVars

parseApp :: Parsec String () Expr
parseApp = do
  exprs <- (between (char '(') (char ')') parseExpr <|> parseVar) `sepEndBy1` many1 space
  return $ foldl1' (:@) exprs

parseVar :: Parsec String () Expr
parseVar = Var <$> parseVarLit

parseVarLit :: Parsec String () String
parseVarLit = many1 alphaNum
