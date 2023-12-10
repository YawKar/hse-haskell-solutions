import Control.Monad.Except
import Data.Char
import Data.List

data ParseError = ParseError {location :: Int, reason :: String}

type ParseMonad = Either ParseError

parseHex :: String -> ParseMonad Integer
parseHex s = do
  when (null s) $ throwError $ ParseError 0 "Empty string"
  case find (not . isHexDigit . fst) (zip s [0 ..]) of
    Just (ch, idx) -> throwError $ ParseError (idx + 1) (ch : ": invalid digit")
    Nothing -> return $ sum $ zipWith (*) (map (fromIntegral . digitToInt) $ reverse s) (map (16 ^) [0 ..])

printError :: ParseError -> ParseMonad String
printError err = return $ "At pos " ++ show (location err) ++ ": " ++ reason err

-- тестирование
test s = str
  where
    (Right str) =
      do
        n <- parseHex s
        return $ show n
        `catchError` printError
