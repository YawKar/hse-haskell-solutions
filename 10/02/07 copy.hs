import System.Random

avgdev'' :: Int -> Int -> Double
avgdev'' k n =
  let t = fromIntegral n / 2 :: Double
      g = mkStdGen 777
      (g', all') = foldr f (g, 0) [1 .. k]
      f _ (g, all's) =
        let (g'', cnt) = e n g
         in (g'', all's + abs (fromIntegral cnt - t))
   in all' / fromIntegral k

e :: (RandomGen g) => Int -> g -> (g, Int)
e l gen = foldr f (gen, 0) [1 .. l]
  where
    f _ (g, s) =
      let (v, g') = randomR (0 :: Int, 1) g
       in (g', v + s)
