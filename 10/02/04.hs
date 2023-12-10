-- Функцию factorial реализовывать не надо!

import Control.Monad qualified
import Data.IORef

while :: IORef a -> (a -> Bool) -> IO () -> IO ()
while ref p action = do
  i <- readIORef ref
  Control.Monad.when (p i) $ do
    action
    while ref p action
