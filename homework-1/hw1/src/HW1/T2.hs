module HW1.T2
  ( N (..)
  , nplus
  , nmult
  , nsub
  , nFromNatural
  , nToNum
  , ncmp
  , nEven
  , nOdd
  , ndiv
  , nmod
  ) where

import Numeric.Natural

data N = Z | S N

nplus :: N -> N -> N
nplus Z y     = y
nplus (S x) y = S (nplus x y)

nmult :: N -> N -> N
nmult Z _     = Z
nmult _ Z     = Z
nmult (S x) y = nplus y (nmult x y)

nsub :: N -> N -> Maybe N
nsub x Z         = Just x
nsub Z _         = Nothing
nsub (S x) (S y) = nsub x y

ncmp :: N -> N -> Ordering
ncmp Z Z         = EQ
ncmp Z _         = LT
ncmp _ Z         = GT
ncmp (S x) (S y) = ncmp x y

nFromNatural :: Natural -> N
nFromNatural 0 = Z
nFromNatural x = S $ nFromNatural $ x - 1

nToNum :: Num a => N -> a
nToNum Z     = 0
nToNum (S x) = 1 + nToNum x

nEven :: N -> Bool
nEven Z     = True
nEven (S x) = not (nEven x)

nOdd :: N -> Bool
nOdd = not . nEven

ndiv :: N -> N -> N
ndiv _ Z       = error "Division by zero"
ndiv num denom = divStep num Z
  where
    divStep restNum result
      | ncmp restNum denom == LT = result
      | otherwise                = divStep (maybe restNum id (nsub restNum denom)) (S result)

nmod :: N -> N -> N
nmod _ Z        = error "Division by zero"
nmod num modulo = modStep num
  where
    modStep restNum
      | ncmp restNum modulo == LT = restNum
      | otherwise                 = modStep $ maybe restNum id $ nsub restNum modulo
