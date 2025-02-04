module HW3.T4
  ( State (..)
  , Prim (..)
  , Expr (..)
  , mapState
  , wrapState
  , joinState
  , modifyState
  , eval
  ) where

import HW3.T1

newtype State s a = S { runS :: s -> Annotated s a }

mapState :: (a -> b) -> State s a -> State s b
mapState f (S run) = S $ \s -> let a :# s' = run s in f a :# s'

wrapState :: a -> State s a
wrapState x = S $ \s -> x :# s

joinState :: State s (State s a) -> State s a
joinState (S runOuter) = S $ \s ->
  let S runInner :# s' = runOuter s
  in runInner s'

modifyState :: (s -> s) -> State s ()
modifyState f = S $ \s -> () :# f s

instance Functor (State s) where
  fmap = mapState

instance Applicative (State s) where
  pure = wrapState
  S f <*> S x = S $ \s ->
    let g :# s' = f s
        y :# s'' = x s'
    in g y :# s''

instance Monad (State s) where
  S x >>= f = S $ \s ->
    let y :# s' = x s
    in runS (f y) s'

data Prim a =
    Add a a
  | Sub a a
  | Mul a a
  | Div a a
  | Abs a
  | Sgn a
  deriving Show

data Expr = Val Double | Op (Prim Expr)
  deriving Show

instance Num Expr where
  x + y = Op (Add x y)
  x - y = Op (Sub x y)
  x * y = Op (Mul x y)
  abs x = Op (Abs x)
  signum x = Op (Sgn x)
  fromInteger x = Val (fromInteger x)

instance Fractional Expr where
  x / y = Op (Div x y)
  fromRational x = Val (fromRational x)

eval :: Expr -> State [Prim Double] Double
eval (Val x) = wrapState x
eval (Op op) = do
  case op of
    Add x y -> evalBinary Add (+) x y
    Sub x y -> evalBinary Sub (-) x y
    Mul x y -> evalBinary Mul (*) x y
    Div x y -> evalBinary Div (/) x y
    Abs x   -> evalUnary Abs abs x
    Sgn x   -> evalUnary Sgn signum x
  where
    evalBinary constructor f x y = do
      x' <- eval x
      y' <- eval y
      let result = f x' y'
      modifyState (constructor x' y' :)
      wrapState result

    evalUnary constructor f x = do
      x' <- eval x
      let result = f x'
      modifyState (constructor x' :)
      wrapState result
