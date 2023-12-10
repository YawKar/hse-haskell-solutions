-- Рукописный логгер
data Logged a = Logged String a deriving (Eq,Show)

instance Functor Logged where
  fmap f (Logged log val) = Logged log (f val)

instance Applicative Logged where
  pure = Logged mempty
  Logged log1 f <*> Logged log2 val = Logged (log2 <> log1) (f val)

instance Monad Logged where
  Logged log val >>= k = Logged (log' <> log) val'
    where
      Logged log' val' = k val

-- эквивалент tell
write2log :: String -> Logged ()
write2log log = Logged log ()
