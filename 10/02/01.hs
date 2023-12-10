import Control.Monad.Writer

minusLoggedR :: (Show a, Num a) => a -> [a] -> Writer String a
minusLoggedR ini [] = do
  tell (show ini)
  return ini
minusLoggedR ini (h : xs) = do
  tell $ "(" <> show h <> "-"
  res <- minusLoggedR ini xs
  tell ")"
  return (h - res)
