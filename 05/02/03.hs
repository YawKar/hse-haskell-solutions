class (Enum a, Bounded a, Eq a) => SafeEnum a where
  ssucc :: a -> a
  ssucc x
    | maxBound == x = minBound
    | otherwise = succ x

  spred :: a -> a
  spred x
    | minBound == x = maxBound
    | otherwise = pred x
