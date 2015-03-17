module Main where

data Fix f = Fix (f (Fix f))

data Expression binOp expr =
    ExprInt   { exprInt :: Int
              }
  | ExprBinOp { exprLeft  :: expr
              , exprBinOp :: binOp
              , exprRight :: expr
              }
--

data BinOp = Add | Sub

type SimpleExpr = Fix (Expression BinOp)

evalSimpleExpr :: SimpleExpr -> Int
evalSimpleExpr (Fix (ExprInt i))        = i
evalSimpleExpr (Fix (ExprBinOp l op r)) = evalL `evalOp` evalR
  where evalL = evalSimpleExpr l
        evalR = evalSimpleExpr r
        evalOp = case op of
                   Add -> (+)
                   Sub -> (-)
--

simple01 :: SimpleExpr
simple01 = Fix (ExprBinOp (Fix (ExprInt 5)) Add (Fix (ExprInt 5)))

main :: IO ()
main = putStrLn "Hello World"
