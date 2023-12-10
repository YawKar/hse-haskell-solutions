import System.Random

avgdev'' :: Int -> Int -> Double
avgdev'' k n =
  let ideal = fromIntegral n / 2 :: Double
      initGen = mkStdGen 42
      (g', total) = foldr f (initGen, 0) (replicate k "")
      f _ (g, derSum) =
        let (g'', count) = oneSerieCount n g
         in (g'', derSum + abs (fromIntegral count - ideal))
   in total / fromIntegral k

oneSerieCount :: (RandomGen g) => Int -> g -> (g, Int)
oneSerieCount len gen = foldr f (gen, 0) (replicate len "")
  where
    f _ (g, s) =
      let (haha, g') = randomR (0 :: Int, 1) g
       in (g', haha + s)
