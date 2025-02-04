-- | This module defines 'ListZipper' datatype.
-- Feel free to define additional helper functions to work
-- with this datatype in this module.
module Data.ListZipper
  ( ListZipper (..)
  , lLeft
  , lRight
  , lGenerator
  , lWrite
  , toList
  ) where

import Control.Comonad (Comonad (..))

-- | A 'ListZipper' is a list of left elements, a focused element,
-- and a list of right elements.
data ListZipper a = LZ [a] a [a]

instance Functor ListZipper where
  fmap f (LZ ls x rs) = LZ (fmap f ls) (f x) (fmap f rs)

instance Comonad ListZipper where
  -- | Extract the focused value.
  extract (LZ _ x _) = x

  -- | Duplicate produces a 'ListZipper' of 'ListZipper's
  -- where each element is itself a focus shift of the original zipper.
  duplicate = lGenerator lLeft lRight

-- | Move the focus one position to the left.
lLeft :: ListZipper a -> ListZipper a
lLeft (LZ (l : ls) x rs) = LZ ls l (x : rs)
lLeft lz                 = lz

-- | Move the focus one position to the right.
lRight :: ListZipper a -> ListZipper a
lRight (LZ ls x (r : rs)) = LZ (x : ls) r rs
lRight lz                 = lz

-- | Helper function that creates an infinite 'ListZipper' by repeatedly
-- applying the given movements to the left and right.
lGenerator :: (a -> a) -> (a -> a) -> a -> ListZipper a
lGenerator f g x = LZ (iterateTail f x) x (iterateTail g x)

-- | Internal helper that produces the tail of infinite iteration.
iterateTail :: (a -> a) -> a -> [a]
iterateTail f = tail . iterate f

-- | Converts the focused region of the 'ListZipper' into a finite list
-- of length 2*n + 1 (the focus plus n elements to the left and right).
toList :: ListZipper a -> Int -> [a]
toList (LZ ls x rs) n = reverse (take n ls) ++ [x] ++ take n rs

-- | Write a value into the focused position of the 'ListZipper'.
lWrite :: a -> ListZipper a -> ListZipper a
lWrite x (LZ ls _ rs) = LZ ls x rs
