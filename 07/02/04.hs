{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE InstanceSigs #-}
{-# OPTIONS_GHC -Wall #-}

newtype Cmps f g x = Cmps {getCmps :: f (g x)}

instance (Functor f, Functor g) => Functor (Cmps f g) where
  fmap :: (Functor f, Functor g) => (a -> b) -> Cmps f g a -> Cmps f g b
  fmap f (Cmps c) = Cmps $ (f <$>) <$> c

instance (Applicative f, Applicative g) => Applicative (Cmps f g) where
  pure = Cmps . pure . pure

  Cmps fgf <*> Cmps fgx = Cmps $ (<*>) <$> fgf <*> fgx
