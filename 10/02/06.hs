import Control.Monad.State
import System.Random

randomRState :: (Random a, RandomGen g) => (a, a) -> State g a
randomRState (x, y) = do
  gen <- get
  let (val, gen') = randomR (x, y) gen
  put gen'
  return val

avgdev' :: Int -> Int -> State StdGen Double
avgdev' k n = do
  let ideal = fromIntegral n / 2
  let oneSerie = do
        experiments <- sequence [randomRState (False, True) | _ <- [1 .. n]]
        let ints = map (\t -> if t then 1 else 0) experiments
        let res = abs $ fromIntegral (sum ints) - ideal
        return res
  probs <- sequence [oneSerie | _serie <- [1 .. k]]
  return $ sum probs / fromIntegral k
