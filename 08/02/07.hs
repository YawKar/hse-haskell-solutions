{-# OPTIONS_GHC -Wall #-}

unit' :: (Applicative f) => f ()
unit' = pure ()

pair' :: (Applicative f) => f a -> f b -> f (a, b)
pair' fa fb = (,) <$> fa <*> fb
