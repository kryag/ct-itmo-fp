module HW2.T3
  ( epart
  , mcat
  ) where

mcat :: Monoid a => [Maybe a] -> a
mcat = foldMap (maybe mempty id)

epart :: (Monoid a, Monoid b) => [Either a b] -> (a, b)
epart = foldMap (\eitherValue -> case eitherValue of
    Left a  -> (a, mempty)
    Right b -> (mempty, b))
