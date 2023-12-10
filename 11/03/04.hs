{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}

import Control.Monad.State
import Data.Functor.Identity

data Logged a = Logged String a deriving (Eq, Show)

newtype LoggT m a = LoggT {runLoggT :: m (Logged a)}

instance (MonadFail m) => MonadFail (LoggT m) where
  fail = LoggT . fail

instance Functor Logged where
  fmap f (Logged log v) = Logged log (f v)

instance Applicative Logged where
  pure = Logged ""
  Logged log0 ab <*> Logged log1 a = Logged (log0 ++ log1) $ ab a

instance (Functor m) => Functor (LoggT m) where
  fmap f l = LoggT $ (f <$>) <$> runLoggT l

instance (Applicative m) => Applicative (LoggT m) where
  pure = LoggT . pure . pure
  l_m_ab <*> l_m_a = LoggT $ (<*>) <$> runLoggT l_m_ab <*> runLoggT l_m_a

instance (Monad m) => Monad (LoggT m) where
  l_m_a >>= k = LoggT $ do
    Logged loga a <- runLoggT l_m_a
    Logged logb b <- runLoggT (k a)
    return (Logged (logb ++ loga) b)

write2log :: (Monad m) => String -> LoggT m ()
write2log s = LoggT $ pure $ Logged s ()

type Logg = LoggT Identity

runLogg :: Logg a -> Logged a
runLogg = runIdentity . runLoggT

instance MonadTrans LoggT where
  -- lift :: (Monad m) => m a -> LoggT m a
  lift m = LoggT $ pure <$> m

instance (MonadState s m) => MonadState s (LoggT m) where
  get = lift get
  put = lift . put
  state = lift . state
