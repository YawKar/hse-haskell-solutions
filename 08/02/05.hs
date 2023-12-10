{-# OPTIONS_GHC -Wall #-}

import Control.Applicative

newtype Parser a = Parser {apply :: String -> [(a, String)]}

instance Functor Parser where
  fmap f (Parser p) = Parser np
    where
      np s = do
        (a, ss) <- p s
        return (f a, ss)

instance Applicative Parser where
  pure a = Parser $ \s -> [(a, s)]
  Parser pab <*> Parser pa = Parser $ \s -> do
    (ab, s') <- pab s
    (a, s'') <- pa s'
    return (ab a, s'')

instance Alternative Parser where
  empty = Parser $ const []
  Parser p1 <|> Parser p2 = Parser $ \s -> p1 s <|> p2 s
