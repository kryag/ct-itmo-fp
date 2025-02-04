{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE TypeOperators        #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE UndecidableInstances #-}

module HW6.T2
  ( TSet

  , Contains
  , Add
  , Delete
  ) where

import GHC.TypeLits

-- | A 'TSet' is simply a type-level list of 'Symbol's.
type TSet = [Symbol]

-- | Checks if a given symbol is in the type-level set.
type family Contains (name :: Symbol) (set :: TSet) :: Bool where
  Contains n (n:xs) = 'True
  Contains n (_:xs) = Contains n xs
  Contains _ '[]    = 'False

-- | Removes the given symbol from the type-level set if it exists;
-- otherwise, leaves the set unchanged.
type family Delete (name :: Symbol) (set :: TSet) :: TSet where
  Delete n (n:xs) = xs
  Delete n (x:xs) = x : Delete n xs
  Delete _ '[]    = '[]

-- | Adds the given symbol to the type-level set, ensuring it is unique
-- by deleting it first, then prepending it to the front.
type family Add (v :: Symbol) (set :: TSet) :: TSet where
  Add n ts = n : Delete n ts
