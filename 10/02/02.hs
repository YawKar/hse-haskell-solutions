{-# LANGUAGE FlexibleContexts #-}

import Control.Monad.Writer

minusLoggedL :: (Show a, Num a) => a -> [a] -> Writer String a
minusLoggedL a ys = minusLoggedL' a (reverse ys)

minusLoggedL' a [] = do
  tell (show a)
  return a
minusLoggedL' a (y : ys) = do
  tell "("
  res <- minusLoggedL' a ys
  tell ("-" ++ show y ++ ")")
  return (res - y)
