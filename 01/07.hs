integration :: (Double -> Double) -> Double -> Double -> Double
integration f a b =
  let n = 1000
      h = (b - a) / n
      xs = map f' [1 .. n - 1]
      f' x = a + h * x
   in h * ((f a + f b) / 2 + sum (map f xs))
