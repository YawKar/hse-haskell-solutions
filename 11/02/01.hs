{-# LANGUAGE FlexibleContexts #-}

import Control.Monad.Except

data ListIndexError
  = ErrTooLargeIndex Int
  | ErrNegativeIndex
  | OtherErr String
  deriving (Eq, Show)

infixl 9 !!!

(!!!) :: (MonadError ListIndexError m) => [a] -> Int -> m a
xs !!! i
  | i < 0 = throwError ErrNegativeIndex
  | null xs = throwError $ ErrTooLargeIndex i
  | i == 0 = return $ head xs
  | otherwise = tail xs !!! pred i `catchError` handler
  where
    handler (ErrTooLargeIndex i') = throwError $ ErrTooLargeIndex i
    handler e = throwError e
