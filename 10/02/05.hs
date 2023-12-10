import System.Random

avgdev :: Int -> Int -> IO Double
avgdev k n = do
  let ideal = fromIntegral n / 2
  let oneSerie = do
        experiments <- sequence [randomIO :: IO Bool | _ <- [1 .. n]]
        let ints = map (\t -> if t then 1 else 0) experiments
        let res = abs $ fromIntegral (sum ints) - ideal
        return res
  probs <- sequence [oneSerie | _serie <- [1 .. k]]
  return $ sum probs / fromIntegral k
