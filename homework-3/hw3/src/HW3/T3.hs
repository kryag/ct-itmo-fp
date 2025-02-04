module HW3.T3
  ( joinOption
  , joinExcept
  , joinAnnotated
  , joinList
  , joinFun
  ) where

import HW3.T1

joinOption :: Option (Option a) -> Option a
joinOption None            = None
joinOption (Some None)     = None
joinOption (Some (Some x)) = Some x

joinExcept :: Except e (Except e a) -> Except e a
joinExcept (Error e)             = Error e
joinExcept (Success (Error e))   = Error e
joinExcept (Success (Success x)) = Success x

joinAnnotated :: Semigroup e => Annotated e (Annotated e a) -> Annotated e a
joinAnnotated ((a :# e1) :# e2) = a :# (e2 <> e1)

joinList :: List (List a) -> List a
joinList Nil               = Nil
joinList (Nil :. xs)       = joinList xs
joinList ((x :. xs) :. ys) = x :. joinList (xs :. ys)

joinFun :: Fun i (Fun i a) -> Fun i a
joinFun (F f) = F (\i -> let (F g) = f i in g i)
