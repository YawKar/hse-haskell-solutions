import Control.Monad.State
fib :: Int -> Integer
fib n = fst $ execState (replicateM n fibStep) (0,1)

fibStep :: State (Integer,Integer) ()
fibStep = do
  (a0, a1) <- get
  put (a1, a0 + a1)
