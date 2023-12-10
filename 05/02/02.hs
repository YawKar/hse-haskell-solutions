import Control.Applicative ((<|>))
import Data.Complex (Complex ((:+)), imagPart, realPart)
import Data.Foldable (traverse_)
import GHC.Read (Read (readPrec))
import Text.Read (ReadPrec, get, pfail)

newtype Cmplx = Cmplx (Complex Double) deriving (Eq)

instance Show Cmplx where
  show (Cmplx c)
    | imagPart c < 0 = show (realPart c) ++ "-i*" ++ show (abs $ imagPart c)
    | otherwise = show (realPart c) ++ "+i*" ++ show (abs $ imagPart c)

readChar :: Char -> ReadPrec Char
readChar c = do
  c' <- get
  if c == c' then return c else pfail

instance Read Cmplx where
  readPrec = do
    number <- readPrec
    imagp <- readImagPart
    return $ Cmplx $ number :+ imagp

readImagPart :: ReadPrec Double
readImagPart = do
  c <- readChar '-' <|> readChar '+'
  traverse_ readChar "i*"
  (if c == '-' then negate else id) <$> readPrec
