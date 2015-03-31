{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies #-}
module SimpleExprAbstract where

import ASTReader ((<?<))
import Control.Monad (guard)

class (BinaryOperator op) => Expression ex op | ex -> op where
  getExprBinOp :: ex -> Maybe (ex, op, ex)
  getExprInt   :: ex -> Maybe Int
--

class BinaryOperator op where
  getBinOp     :: op -> Maybe BinOp
--

data BinOp = Add | Sub
  deriving (Show, Eq)
--

elimSide :: (Expression ex op) =>
              (ex -> ex -> ex)
           -> (Int -> op -> Maybe Bool)
           -> ex
           -> Maybe ex
elimSide focal p ex =  do
  (l, op, r) <- getExprBinOp ex
  i <- getExprInt (focal l r)
  guard =<< p i op
  return (focal r l)
--

elimZeroAdd :: (Expression ex op) => ex -> Maybe ex
elimZeroAdd = elimSide (\l _ -> l) p <?< elimSide (\_ r -> r) p
  where p i op = do
          binOp <- getBinOp op
          return $ i == 0 && binOp == Add
--

elimZeroSub :: (Expression ex op) => ex -> Maybe ex
elimZeroSub = elimSide (\_ r -> r) p
  where p i op = do
          binOp <- getBinOp op
          return $ i == 0 && binOp == Sub
--

optimise :: (Expression ex op) => ex -> ex
optimise ex = maybe ex id (o ex)
  where o = elimZeroSub <?< elimZeroAdd
--
