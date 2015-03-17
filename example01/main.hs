module Main where

data Fix f = Fix (f (Fix f))

--
-- Base AST template
--

data Expression binOp expr =
    ExprInt   { exprInt :: Int
              }
  | ExprBinOp { exprLeft  :: expr
              , exprBinOp :: binOp
              , exprRight :: expr
              }
--

data BinOp = Add | Sub


--
-- Extending with more binary operations
--

data BinOpExt = Mul | Div | BinOpBase BinOp


--
-- Extending with unary operations
--

data ExpressionExt uniOp binOp expr =
    ExprUniOp { exprUniOp :: uniOp
              , exprArg   :: expr
              }
  | ExprBase (Expression binOp expr)
--

data UniOp = Fac | Log


--
-- Instantiation of the base AST.
--

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


--
-- Instantiation of the extended AST.
--

type ExtendedExpr = Fix (ExpressionExt UniOp BinOpExt)

evalExtendedExpr :: ExtendedExpr -> Int
evalExtendedExpr (Fix (ExprUniOp op arg)) = evalOp evalArg
  where evalOp = case op of
                   Fac -> product . flip take [1..]
                   Log -> floor . log . fromIntegral
        evalArg = evalExtendedExpr arg
evalExtendedExpr (Fix (ExprBase (ExprInt i))) = i
evalExtendedExpr (Fix (ExprBase (ExprBinOp l op r))) = evalL `evalOp` evalR
  where evalL = evalExtendedExpr l
        evalR = evalExtendedExpr r
        evalOp = case op of
                   Mul -> (*)
                   Div -> div
                   BinOpBase Add -> (+)
                   BinOpBase Sub -> (-)
--

extended01 :: ExtendedExpr
extended01 = Fix (ExprUniOp Fac (Fix (ExprBase (ExprBinOp (Fix (ExprBase (ExprInt 3))) Mul (Fix (ExprBase (ExprInt 2)))))))


-- This is to make the compiler happy.

main :: IO ()
main = putStrLn "Hello World"
