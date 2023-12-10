{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}

import Control.Applicative
import Control.Monad.Except

data Excep a = Err String | Ok a
  deriving (Eq, Show)

instance Functor Excep where
  fmap f (Ok a) = Ok $ f a
  fmap _ (Err s) = Err s

instance Applicative Excep where
  pure = Ok
  (Ok f) <*> (Ok a) = Ok $ f a
  (Err s) <*> _ = Err s
  _ <*> (Err s) = Err s

instance Monad Excep where
  return = pure
  (Ok a) >>= f = f a
  (Err s) >>= _ = Err s

instance (MonadError String) Excep where
  throwError = Err
  catchError (Err s) f = f s
  catchError (Ok a) _ = Ok a

instance MonadFail Excep where
  fail = Err

instance Alternative Excep where
  empty = Err "Alternative.empty error."
  (<|>) (Err _) b = b
  (<|>) a _ = a

instance MonadPlus Excep where
  mzero = Err "MonadPlus.mzero error."
  mplus (Err _) b = b
  mplus a _ = a

-- тестирование
(?/) ::
  (MonadError String m) =>
  Double ->
  Double ->
  m Double
x ?/ 0 = throwError "Division by 0."
x ?/ y = return $ x / y

example :: Double -> Double -> Excep String
example x y = action `catchError` return
  where
    action = do
      q <- x ?/ y
      guard (q >= 0)
      when (q > 100) $ throwError "Monad.fail error."
      return $ show q
