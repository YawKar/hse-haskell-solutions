{-# LANGUAGE InstanceSigs #-}

import Control.Monad.Identity (Identity (..), ap)

newtype StrRdrT m a = StrRdrT {runStrRdrT :: String -> m a}

instance (Monad m) => Monad (StrRdrT m) where
  return :: a -> StrRdrT m a
  return = StrRdrT . const . pure

  (>>=) :: StrRdrT m a -> (a -> StrRdrT m b) -> StrRdrT m b
  m >>= k = StrRdrT $ \s ->
    let r = runStrRdrT m s
     in r >>= (\a -> runStrRdrT (k a) s)

instance (MonadFail m) => MonadFail (StrRdrT m) where
  fail :: String -> StrRdrT m a
  fail e = StrRdrT $ \_ -> fail e

instance (Monad m) => Functor (StrRdrT m) where
  fmap f (StrRdrT origF) = StrRdrT (fmap f . origF)

instance (Monad m) => Applicative (StrRdrT m) where
  pure = return
  (<*>) = ap
