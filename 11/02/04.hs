import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Except
import Data.Char (isNumber, isPunctuation)
import Data.Foldable (msum)

{- Не снимайте комментарий - эти объявления даны в вызывающем коде
newtype PwdError = PwdError String

type PwdErrorIOMonad = ExceptT PwdError IO

askPassword :: PwdErrorIOMonad ()
askPassword = do
  liftIO $ putStrLn "Enter your new password:"
  value <- msum $ repeat getValidPassword
  liftIO $ putStrLn "Storing in database..."
-}

instance Semigroup PwdError where
  PwdError e1 <> PwdError e2 = PwdError (e1 <> e2)

instance Monoid PwdError where
  mempty = PwdError ""

getValidPassword :: PwdErrorIOMonad String
getValidPassword = do
  s <- liftIO getLine
  if length s < 8
    then printAndThrow "Incorrect input: password is too short!"
    else
      if not (any isNumber s)
        then printAndThrow "Incorrect input: password must contain some digits!"
        else
          if not (any isPunctuation s)
            then printAndThrow "Incorrect input: password must contain some punctuations!"
            else return s
  where
    printAndThrow s = do
      liftIO $ putStrLn s
      throwE $ PwdError s
