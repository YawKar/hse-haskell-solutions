{-# LANGUAGE InstanceSigs #-}
{-# OPTIONS_GHC -Wall #-}

class (Functor f) => Monoidal f where
  unit :: f ()
  (*&*) :: f a -> f b -> f (a, b)

instance Monoidal Maybe where
  unit :: Maybe ()
  unit = Just ()

  (*&*) :: Maybe a -> Maybe b -> Maybe (a, b)
  Nothing *&* _ = Nothing
  _ *&* Nothing = Nothing
  Just a *&* Just b = Just (a, b)

instance (Monoid s) => Monoidal ((,) s) where
  unit :: (s, ())
  unit = (mempty, ())

  (*&*) :: (s, a) -> (s, b) -> (s, (a, b))
  (s0, a) *&* (s1, b) = (s0 <> s1, (a, b))

instance Monoidal ((->) e) where
  unit :: e -> ()
  unit _ = ()

  (*&*) :: (e -> a) -> (e -> b) -> (e -> (a, b))
  ea *&* eb = \e -> (ea e, eb e)
