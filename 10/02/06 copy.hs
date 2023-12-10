import Control.Monad.State
import System.Random

randomRState :: (Random a, RandomGen g) => (a, a) -> State g a
randomRState (x, y) = do
  g <- get
  let (v, g') = randomR (x, y) g
  put g'
  return v

avgdev' :: Int -> Int -> State StdGen Double
avgdev' k n = do
  let t = fromIntegral n / 2
  let e = do
        es <- sequence [randomRState (False, True) | _ <- [1 .. n]]
        return $ abs $ fromIntegral (sum $ map (\bl -> if bl then 1 else 0) es) - t
  ss <- sequence [e | _ <- [1 .. k]]
  return $ sum ss / fromIntegral k
