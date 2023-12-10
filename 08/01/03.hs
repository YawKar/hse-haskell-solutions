-- {-# LANGUAGE InstanceSigs #-}
-- {-# OPTIONS_GHC -Wall #-}

-- -- BOILERPLATE
-- import Control.Applicative (Alternative (empty, some, (<|>)))
-- import Data.Char (digitToInt, isDigit)

-- newtype Parser t a = Parser {runParser :: [t] -> Maybe ([t], a)}

-- instance Functor (Parser t) where
--   fmap :: (a -> b) -> Parser t a -> Parser t b
--   fmap f (Parser p) = Parser $ \t -> case p t of
--     Nothing -> Nothing
--     Just (t', a) -> Just (t', f a)

-- instance Applicative (Parser t) where
--   pure :: a -> Parser t a
--   pure = Parser . fmap Just . flip (,)

--   (<*>) :: Parser t (a -> b) -> Parser t a -> Parser t b
--   Parser pfab <*> pa = Parser $ \t -> case pfab t of
--     Nothing -> Nothing
--     Just (t', fab) -> runParser (fab <$> pa) t'

-- instance Alternative (Parser t) where
--   empty :: Parser t a
--   empty = Parser $ const Nothing

--   (<|>) :: Parser t a -> Parser t a -> Parser t a
--   Parser p1 <|> p2 = Parser $ \t -> case p1 t of
--     Nothing -> runParser p2 t
--     res -> res

-- satisfy :: (a -> Bool) -> Parser a a
-- satisfy pr = Parser p
--   where
--     p (a : as) | pr a = Just (as, a)
--     p _ = Nothing

-- digit :: Parser Char Int
-- digit = digitToInt <$> satisfy isDigit

-- -- END OF BOILERPLATE

nat :: Parser Char Int
nat = read . concatMap show <$> some digit
