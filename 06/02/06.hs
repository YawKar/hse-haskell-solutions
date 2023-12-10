{-# LANGUAGE InstanceSigs #-}
{-# OPTIONS_GHC -Wall #-}

import Data.Maybe (mapMaybe)

data Tree a = Nil | Branch (Tree a) a (Tree a) deriving (Eq, Show)

newtype Preorder a = PreO (Tree a) deriving (Eq, Show)

newtype Postorder a = PostO (Tree a) deriving (Eq, Show)

newtype Levelorder a = LevelO (Tree a) deriving (Eq, Show)

instance Foldable Tree where
  foldr :: (a -> b -> b) -> b -> Tree a -> b
  foldr _ ini Nil = ini
  foldr f ini (Branch left x right) = foldr f (x `f` foldr f ini right) left

instance Foldable Levelorder where
  foldr :: (a -> b -> b) -> b -> Levelorder a -> b
  foldr _ ini (LevelO Nil) = ini
  foldr f ini (LevelO t) = foldr f ini (bfs [t] [])
    where
      bfs :: [Tree a] -> [a] -> [a]
      bfs [] acc = reverse acc
      bfs roots acc = bfs nextRoots newAcc
        where
          nextRoots = concatMap flatTree roots
          newAcc = reverse curRootsValues ++ acc
          curRootsValues = mapMaybe toValue roots

      flatTree :: Tree a -> [Tree a]
      flatTree Nil = []
      flatTree (Branch left _ right) = [left, right]

      toValue :: Tree a -> Maybe a
      toValue Nil = Nothing
      toValue (Branch _ x _) = Just x

treeToListInOrder :: Levelorder a -> [a]
treeToListInOrder = foldr (:) []

instance Foldable Preorder where
  foldr :: (a -> b -> b) -> b -> Preorder a -> b
  foldr _ ini (PreO Nil) = ini
  foldr f ini (PreO (Branch left x right)) = x `f` foldr f (foldr f ini right') left'
    where
      left' = PreO left
      right' = PreO right

treeToListPreOrder :: Preorder a -> [a]
treeToListPreOrder = foldr (:) []

instance Foldable Postorder where
  foldr :: (a -> b -> b) -> b -> Postorder a -> b
  foldr _ ini (PostO Nil) = ini
  foldr f ini (PostO (Branch left x right)) = foldr f (foldr f (x `f` ini) right') left'
    where
      left' = PostO left
      right' = PostO right

treeToListPostOrder :: Postorder a -> [a]
treeToListPostOrder = foldr (:) []
