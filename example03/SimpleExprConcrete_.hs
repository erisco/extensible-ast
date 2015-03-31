module SimpleExprConcrete where

import qualified SimpleExprAbstract as SmplAbs
import SimpleExprAbstract (BinOp(Add,Sub), getGetExprInt, getGetExprBinOp)

data Fix f = Fix (f (Fix f))

data Expression binOp expr =
    ExprInt   { exprInt :: Int
              }
  | ExprBinOp { exprLeft  :: expr
              , exprBinOp :: binOp
              , exprRight :: expr
              }
--

type SimpleExpr = Fix (Expression BinOp)


simpleViewers :: SmplAbs.Viewers SimpleExpr BinOp
simpleViewers = SmplAbs.Viewers
  { getGetExprInt   = getExprInt
  , getGetExprBinOp = getExprBinOp
  }
  where getExprInt (Fix (ExprInt i)) = Just i
        getExprInt _                 = Nothing
        getExprBinOp (Fix (ExprBinOp l op r)) = Just (l, op, r)
        getExprBinOp _                        = Nothing
--

deepOptimise :: SimpleExpr -> SimpleExpr
deepOptimise ex =
  case ex of
    Fix (ExprBinOp l op r) -> opt $ Fix (ExprBinOp (opt l) op (opt r))
    x                      -> x
  where opt = SmplAbs.optimise simpleViewers
--

simBinOp :: SimpleExpr -> BinOp -> SimpleExpr -> SimpleExpr
simBinOp l op r = Fix (ExprBinOp l op r)

simInt :: Int -> SimpleExpr
simInt i = Fix (ExprInt i)

simAdd :: SimpleExpr -> SimpleExpr -> SimpleExpr
simAdd l r = simBinOp l Add r

simSub :: SimpleExpr -> SimpleExpr -> SimpleExpr
simSub l r = simBinOp l Sub r


printSimpleExpr :: SimpleExpr -> String
printSimpleExpr (Fix (ExprBinOp l op r)) = "(" ++ printSimpleExpr l ++ " "
  ++ show op ++ " " ++ printSimpleExpr r ++ ")"
printSimpleExpr (Fix (ExprInt i))        = show i

simple01 :: SimpleExpr
simple01 = (simInt 0 `simAdd` simInt 5) `simAdd` (simInt 0 `simSub` simInt 0)
