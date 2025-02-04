module HW2.T2
  ( joinWith
  , splitOn
  ) where

import Data.List.NonEmpty (NonEmpty(..))

splitOn :: Eq a => a -> [a] -> NonEmpty [a]
splitOn _ []   = [] :| []
splitOn sep xs = foldr splitStep ([] :| []) xs
  where
    splitStep c (cur :| rest)
      | c == sep  = [] :| (cur : rest)
      | otherwise = (c : cur) :| rest

joinWith :: a -> NonEmpty [a] -> [a]
joinWith _ (x :| [])       = x
joinWith sep (cur :| rest) = cur ++ foldr (\x acc -> sep : x ++ acc) [] rest
