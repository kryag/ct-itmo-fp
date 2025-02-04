module HW4.T1
  ( EvaluationError (..)
  , ExceptState (..)
  , mapExceptState
  , wrapExceptState
  , joinExceptState
  , modifyExceptState
  , throwExceptState
  , eval
  ) where

import HW4.Types

data ExceptState e s a = ES { runES :: s -> Except e (Annotated s a) }

mapExceptState :: (a -> b) -> ExceptState e s a -> ExceptState e s b
mapExceptState f (ES run) = ES $ \s ->
  case run s of
    Error e           -> Error e
    Success (a :# s') -> Success (f a :# s')

wrapExceptState :: a -> ExceptState e s a
wrapExceptState x = ES $ \s -> Success (x :# s)

joinExceptState :: ExceptState e s (ExceptState e s a) -> ExceptState e s a
joinExceptState (ES runOuter) = ES $ \s ->
  case runOuter s of
    Error e               -> Error e
    Success (inner :# s') -> runES inner s'

modifyExceptState :: (s -> s) -> ExceptState e s ()
modifyExceptState f = ES $ \s -> Success (() :# f s)

throwExceptState :: e -> ExceptState e s a
throwExceptState e = ES $ \_ -> Error e

instance Functor (ExceptState e s) where
  fmap = mapExceptState

instance Applicative (ExceptState e s) where
  pure          = wrapExceptState
  ES f <*> ES x = ES $ \s ->
    case f s of
      Error e           -> Error e
      Success (g :# s') ->
        case x s' of
          Error e'           -> Error e'
          Success (y :# s'') -> Success (g y :# s'')

instance Monad (ExceptState e s) where
  ES x >>= f = ES $ \s ->
    case x s of
      Error e           -> Error e
      Success (y :# s') -> runES (f y) s'

data EvaluationError = DivideByZero
  deriving Show

eval :: Expr -> ExceptState EvaluationError [Prim Double] Double
eval (Val x) = wrapExceptState x
eval (Op op) =
  case op of
    Add x y -> evalBinary Add (+) x y
    Sub x y -> evalBinary Sub (-) x y
    Mul x y -> evalBinary Mul (*) x y
    Div x y -> do
      x' <- eval x
      y' <- eval y
      if y' == 0
        then throwExceptState DivideByZero
        else evalBinary Div (/) (Val x') (Val y')
    Abs x   -> evalUnary Abs abs x
    Sgn x   -> evalUnary Sgn signum x
  where
    evalBinary constructor f x y = do
      x' <- eval x
      y' <- eval y
      let result = f x' y'
      modifyExceptState (constructor x' y' :)
      wrapExceptState result

    evalUnary constructor f x = do
      x' <- eval x
      let result = f x'
      modifyExceptState (constructor x' :)
      wrapExceptState result
