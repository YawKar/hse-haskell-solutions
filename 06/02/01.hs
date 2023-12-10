{-# OPTIONS_GHC -Wall #-}

import Data.Bifunctor (Bifunctor (second))
import Data.List (unfoldr)

revRange :: (Char, Char) -> [Char]
revRange = unfoldr fun

fun :: (Char, Char) -> Maybe (Char, (Char, Char))
fun lims@(from, lim)
  | from < lim = Just (lim, second pred lims)
  | from == lim = Just (lim, ('b', 'a')) -- because `pred '\NUL'` panics
  | otherwise = Nothing
