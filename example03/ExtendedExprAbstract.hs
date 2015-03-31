{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies #-}
module ExtendedExprAbstract where

import ASTReader ((<?<))
import qualified SimpleExprAbstract as Smpl

data BinOp = Mul | Div
  deriving (Show, Eq)
--

data UniOp = Fac | Log

class ( BinaryOperator binOp
      , UnaryOperator uniOp
      , Smpl.Expression ex binOp
      ) => Expression ex uniOp binOp | ex -> uniOp binOp where
  getExprUniOp :: ex -> Maybe (uniOp, ex)
--

class (Smpl.BinaryOperator op) => BinaryOperator op where
  getBinOp :: op -> Maybe BinOp
--

class UnaryOperator op where
  getUniOp :: op -> Maybe UniOp
--

elimMulOne :: (Expression ex uniOp binOp) => ex -> Maybe ex
elimMulOne = Smpl.elimSide (\l _ -> l) p <?< Smpl.elimSide (\r _ -> r) p
  where p i op = do
          binOp <- getBinOp op
          return $ i == 1 && binOp == Mul
--

elimDivOne :: (Expression ex uniOp binOp) => ex -> Maybe ex
elimDivOne = Smpl.elimSide (\_ r -> r) p
  where p i op = do
          binOp <- getBinOp op
          return $ i == 1 && binOp == Div

optimise :: (Expression ex uniOp binOp) => ex -> ex
optimise ex = maybe ex id (o ex)
  where o = elimMulOne <?< elimDivOne
--
