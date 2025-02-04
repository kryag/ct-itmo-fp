-- | This module defines 'Grid' datatype.
-- Feel free to define additional helper functions to work
-- with this datatype in this module.
module Data.Grid
  ( Grid (..)
  , gUp
  , gDown
  , gLeft
  , gRight
  , gWrite
  ) where

import Control.Comonad (Comonad (..))

import Data.ListZipper (ListZipper (..), lLeft, lRight, lGenerator, lWrite)

-- | A 'Grid' is essentially a 'ListZipper' of 'ListZipper's, representing
-- an infinite 2D focusable structure.
newtype Grid a = Grid { unGrid :: ListZipper (ListZipper a) }

instance Functor Grid where
  fmap f (Grid g) = Grid $ fmap (fmap f) g

instance Comonad Grid where
  -- | Extract the focused element in the 2D grid.
  extract = extract . extract . unGrid

  -- | Duplicate the grid, providing a grid of all possible focuses.
  duplicate = Grid . fmap gHorizontal . gVertical

-- | Generate a zipper of 'Grid's by moving
-- left and right (horizontal) or up and down (vertical).
gHorizontal, gVertical :: Grid a -> ListZipper (Grid a)
gHorizontal = lGenerator gLeft gRight
gVertical   = lGenerator gUp   gDown

-- | Move the grid focus one cell up/down.
gUp, gDown :: Grid a -> Grid a
gUp   (Grid g) = Grid (lLeft  g)
gDown (Grid g) = Grid (lRight g)

-- | Move the grid focus one cell to the left/right.
gLeft, gRight :: Grid a -> Grid a
gLeft  (Grid g) = Grid (fmap lLeft  g)
gRight (Grid g) = Grid (fmap lRight g)

-- | Write a new value to the currently focused cell in the grid.
gWrite :: a -> Grid a -> Grid a
gWrite x (Grid g) = Grid $ lWrite newLine g
  where
    oldLine = extract g
    newLine = lWrite x oldLine
