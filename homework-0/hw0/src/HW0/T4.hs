module HW0.T4
  ( fac
  , fib
  , map'
  , repeat'
  ) where

import Data.Function (fix)
import Numeric.Natural (Natural)

repeat' :: a -> [a]
repeat' x = fix (x:)

map' :: (a -> b) -> [a] -> [b]
map' = fix (\rec f lst -> if null lst then [] else f (head lst) : rec f (tail lst))

fib :: Natural -> Natural
fib = fix (\rec prev cur n -> case n of
  0 -> prev
  1 -> cur
  _ -> rec cur (prev + cur) (n - 1)) 0 1

fac :: Natural -> Natural
fac = fix (\rec n -> if n <= 1 then 1 else n * rec (n - 1))
