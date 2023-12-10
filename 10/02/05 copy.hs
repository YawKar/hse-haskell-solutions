import System.Random

avgdev :: Int -> Int -> IO Double
avgdev k n = do
  let t = fromIntegral n / 2
  let e = do
        es <- sequence [randomIO :: IO Bool | _ <- [1 .. n]]
        return $ abs $ fromIntegral (sum $ map (\bl -> if bl then 1 else 0) es) - t
  ss <- sequence [e | _ <- [1 .. k]]
  return $ sum ss / fromIntegral k
