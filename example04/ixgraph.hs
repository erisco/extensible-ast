module IxGraph where

-- Trees with unique node indices, efficient merges.

import Prelude hiding (lookup)

data Tree a =
    Branch { left  :: Tree a
           , label :: a
           , right :: Tree a
           , sizeL :: Int
           , sizeR :: Int
           }
  | Leaf
  deriving (Show)
--

empty :: Tree a
empty = Leaf

singleton :: a -> Tree a
singleton a = Branch empty a empty 0 0

size :: Tree a -> Int
size Leaf   = 0
size branch = sizeL branch + sizeR branch + 1

merge :: (Ord a) => Tree a -> Tree a -> Tree a
merge Leaf y = y
merge x Leaf = x
merge x y
  | label x <= label y = Branch { left  = merge x (left y)
                                , label = label y
                                , right = right y
                                , sizeL = size (merge x (left y))
                                , sizeR = sizeR y
                                }
  | otherwise = Branch { left = merge y (left x)
                       , label = label x
                       , right = right x
                       , sizeL = size (merge y (left x))
                       , sizeR = sizeR x
                       }
--

lookup :: Int -> Tree a -> Maybe a
lookup _ Leaf = Nothing
lookup ix branch
  | ix == sizeL branch = Just (label branch)
  | ix < sizeL branch  = lookup ix (left branch)
  | otherwise = lookup (ix - sizeL branch) (right branch)
--

example :: Tree Char
example = (singleton 'a') `merge` (singleton 'b') `merge` (singleton 'c')
