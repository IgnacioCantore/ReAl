module Monad where

import           Common
import qualified Data.Map                      as M
import           Control.Monad                  ( liftM
                                                , ap
                                                )

-- Clases de mónadas que proveen las operaciones necesarias para implementar
-- los evaluadores

-- Clase para representar mónadas con estado de variables y con manejo de errores
class Monad m => MonadStateError m where
    -- Busca el valor de una variable
    lookfor :: Name -> m (Either Table Operator)
    -- Lanza un error
    throw :: String -> m a

-- Mónada estado, con manejo de errores
newtype StateError a =
  StateError { runStateError :: Env -> Either String a }

-- Para calmar al GHC
instance Functor StateError where
  fmap = liftM

instance Applicative StateError where
  pure  = return
  (<*>) = ap

instance Monad StateError where
  return x = StateError (\s -> Right x)
  m >>= f = StateError (\s -> case runStateError m s of
                                   Left r  -> Left r
                                   Right a -> runStateError (f a) s)

instance MonadStateError StateError where
  lookfor v = StateError (\s -> case M.lookup v s of
                                  Nothing        -> Left ("La variable " ++ v ++ " no está definida")
                                  Just (Left t)  -> Right (Left t)
                                  Just (Right r) -> Right (Right r))
  throw s = StateError (\_ -> Left s)
