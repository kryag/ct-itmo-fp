module HW1.T3
  ( Tree (..)
  , tsize
  , tdepth
  , tmember
  , tinsert
  , tFromList
  ) where

type Meta = (Int, Int) -- (size, depth)

data Tree a = Leaf | Branch Meta (Tree a) a (Tree a)
  deriving (Show)

tsize :: Tree a -> Int
tsize Leaf                     = 0
tsize (Branch (size, _) _ _ _) = size

tdepth :: Tree a -> Int
tdepth Leaf                      = 0
tdepth (Branch (_, depth) _ _ _) = depth

tmember :: Ord a => a -> Tree a -> Bool
tmember _ Leaf = False
tmember searchValue (Branch _ left value right)
  | searchValue == value = True
  | searchValue < value  = tmember searchValue left
  | otherwise            = tmember searchValue right

tinsert :: Ord a => a -> Tree a -> Tree a
tinsert insertValue Leaf = mkBranch Leaf insertValue Leaf
tinsert insertValue branch@(Branch _ left value right)
  | insertValue == value = branch
  | insertValue < value  = balance $ mkBranch (tinsert insertValue left) value right
  | otherwise            = balance $ mkBranch left value $ tinsert insertValue right

tFromList :: Ord a => [a] -> Tree a
tFromList = foldr tinsert Leaf

mkBranch :: Tree a -> a -> Tree a -> Tree a
mkBranch left value right = Branch meta left value right
  where
    meta = (1 + tsize left + tsize right, 1 + max (tdepth left) (tdepth right))

balance :: Tree a -> Tree a
balance Leaf = Leaf
balance node@(Branch _ left _ right)
  | factorNode > 1  = if factorLeft < 0 then rotateRightLeft node else rotateRight node
  | factorNode < -1 = if factorRight > 0 then rotateLeftRight node else rotateLeft node
  | otherwise       = node
  where
    factorNode  = balanceFactor node
    factorLeft  = balanceFactor left
    factorRight = balanceFactor right

balanceFactor :: Tree a -> Int
balanceFactor Leaf                    = 0
balanceFactor (Branch _ left _ right) = tdepth left - tdepth right

rotateRight :: Tree a -> Tree a
rotateRight (Branch _ (Branch _ ll lv lr) v right) = mkBranch ll lv (mkBranch lr v right)
rotateRight branch                                 = branch

rotateLeft :: Tree a -> Tree a
rotateLeft (Branch _ left v (Branch _ rl rv rr)) = mkBranch (mkBranch left v rl) rv rr
rotateLeft branch                                = branch

rotateLeftRight :: Tree a -> Tree a
rotateLeftRight (Branch _ left v right) = rotateLeft $ mkBranch left v $ rotateRight right
rotateLeftRight branch                  = branch

rotateRightLeft :: Tree a -> Tree a
rotateRightLeft (Branch _ left v right) = rotateRight $ mkBranch (rotateLeft left) v right
rotateRightLeft branch                  = branch
