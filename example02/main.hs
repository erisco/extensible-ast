module Main where

--
-- BASE LANGUAGE
-- The base language includes integers, addition, and subtraction.
--

--
-- EXTENDED LANGUAGE
-- The extended language includes the base language plus factorial, log (base
-- e), multiplication, and division.
--

--------------------------------------------------------------------------------
-- BASE LANGUAGE evaluation
--
-- This is how expressions of the base language are evaluated. Note that none
-- of these functions are particular to an AST or ADT; they only hint at
-- necessary features.
--------------------------------------------------------------------------------


-- | Evaluates a binary operation expression.
evalExprBinOp ::
                    (binOp -> Int -> Int -> Int)
                    -- ^ How to evaluate binary operators
                 -> (expr -> Int)
                    -- ^ How to evaluate expressions
                 -> expr
                    -- ^ Left argument
                 -> binOp
                    -- ^ Binary operator
                 -> expr
                    -- ^ Right argument
                 -> Int
                    -- ^ Evaluation returns some Int
evalExprBinOp evalBinOp' evalExpr' l op r
  = evalBinOp' op (evalExpr' l) (evalExpr' r)
--


-- | Evaluates an integer value.
evalExprInt :: Int -> Int
evalExprInt i = i


-- | Evaluate an integer addition operator.
evalAdd :: Int -> Int -> Int
evalAdd = (+)


-- | Evaluate an integer subtraction operator.
evalSub :: Int -> Int -> Int
evalSub = (-)


--------------------------------------------------------------------------------
-- EXTENDED LANGUAGE evaluation
--
-- This is how expressions of the extended language are evaluated. Note that
-- none of these functions are particular to an AST or ADT, nor do they need to
-- reference the base language; they only hint at necessary features and
-- only explain how to evaluate what is unique to the extended language.
--------------------------------------------------------------------------------


-- | Evalute a unary integer operation.
evalExprUniOp ::    (uniOp -> Int -> Int)
                    -- ^ How to evaluate unary operators
                 -> (expr -> Int)
                    -- ^ How to evaluate expressions
                 -> uniOp
                    -- ^ Unary operator
                 -> expr
                    -- ^ Argument
                 -> Int
                    -- ^ Evaluation returns some Int
evalExprUniOp evalUniOp' evalExpr' op arg = evalUniOp' op (evalExpr' arg)


-- | Evaluate an integer multiplication operator.
evalMul :: Int -> Int -> Int
evalMul = (*)


-- | Evaluate an integer division operator.
evalDiv :: Int -> Int -> Int
evalDiv = div


-- | Evaluates an integer factorial operator.
evalFac :: Int -> Int
evalFac = product . flip take [1..]


-- | Evaluates an integer log operator.
evalLog :: Int -> Int
evalLog = floor . log . fromIntegral



--------------------------------------------------------------------------------
-- BASE LANGUAGE representation and evaluation
--
-- The base language can be represented using ADTs. To permit the extended
-- language representation to reuse the base language representation type
-- parameters are added.
--
-- Evaluating the ADT only requires case analysis and application of the
-- predefined evaluation logic.
--------------------------------------------------------------------------------


-- | BASE LANGUAGE expressions
data Expression binOp expr =
    ExprInt   { exprInt :: Int
              }
  | ExprBinOp { exprLeft  :: expr
              , exprBinOp :: binOp
              , exprRight :: expr
              }
--


-- | BASE LANGUAGE binary operators
data BinOp = Add | Sub


-- | Evaluate an `Expression` value. Note that this is simply applying
--   `evalExprBinOp` and `evalExprInt` to the `Expression` ADT.
evalExpr ::    (binOp -> Int -> Int -> Int)
               -- ^ How to evaluate binary operators
            -> (expr -> Int)
               -- ^ How to evaluate expressions
            -> Expression binOp expr
               -- ^ `Expression` value to evaluate
            -> Int
               -- ^ Evaluation returns some Int
evalExpr evalBinOp' evalExpr' expr =
  case expr of
    ExprInt i        -> evalExprInt i
    ExprBinOp l op r -> evalExprBinOp evalBinOp' evalExpr' l op r
--


-- | Evaluate a `BinOp` value. Note that this is simply applying `evalAdd` and
--   `evalSub` to the `BinOp` ADT.
evalBinOp :: BinOp -> Int -> Int -> Int
evalBinOp Add = evalAdd
evalBinOp Sub = evalSub



--------------------------------------------------------------------------------
-- EXTENDED LANGUAGE representation and evaluation
--
-- The extended language can also be represented using ADTs. Because the base
-- language representation is parameterized the extended language representation
-- can reuse it. An alternative approach is to replicate the features of the
-- base language representation in the extended language representation.
--
-- The extended language representation is parameterized so that it can be
-- further extended in similar fashion.
--
-- Evaluating the ADT only requires case analysis and application of the
-- predefined evaluation logic.
--------------------------------------------------------------------------------

-- | EXTENDED LANGUAGE expressions
data ExpressionExt uniOp binOp expr =
    ExprUniOp { exprUniOp :: uniOp
              , exprArg   :: expr
              }
  | ExprBase (Expression binOp expr)
--

-- | EXTENDED LANGUAGE binary operators
data BinOpExt = Mul | Div | BinOpBase BinOp


-- | EXTENDED LANGUAGE unary operators
data UniOp = Fac | Log


-- | Evaluate a `ExpressionExt` value. Note that this simply applies
--   `evalExprUniOp` and `evalExpr` to the `ExpressionExt` ADT.
evalExprExt ::    (uniOp -> Int -> Int)
                  -- ^ How to evaluate unary operators
               -> (binOp -> Int -> Int -> Int)
                  -- ^ How to evaluate binary operators
               -> (expr -> Int)
                  -- ^ How to evaluate expressions
               -> ExpressionExt uniOp binOp expr
                  -- ^ Expression
               -> Int
                  -- ^ Evaluation returns some Int
evalExprExt evalUniOp' evalBinOp' evalExpr' expr =
  case expr of
    ExprUniOp op arg -> evalUniOp' op (evalExpr' arg)
    ExprBase base    -> evalExpr evalBinOp' evalExpr' base
--


-- | Evaluate a `BinOpExt` value. Note that this is simply applying `evalMul`,
--   `evalDiv`, and `evalBinOp` to the `BinOpExt` ADT.
evalBinOpExt :: BinOpExt -> Int -> Int -> Int
evalBinOpExt Mul = evalMul
evalBinOpExt Div = evalDiv
evalBinOpExt (BinOpBase binOp) = evalBinOp binOp


-- | Evaluates a `UniOp` value. Note that this simply applies `evalFac` and
--   `evalLog` to the `UniOp` ADT.
evalUniOp :: UniOp -> Int -> Int
evalUniOp Fac = evalFac
evalUniOp Log = evalLog


--------------------------------------------------------------------------------
-- BASE LANGUAGE representation instantiation, evaluation, and example
--
-- A concrete base language representation can be instantiated by choosing
-- BinOp for binary operators and choosing similar recursive expressions (done
-- by Fix).
--
-- Evaluating the instantiated base language representation only requires using
-- the predefined evaluation logic for the parameterized ADTs.
--------------------------------------------------------------------------------


-- | Instantiation of BASE LANGUAGE representation.
type SimpleExpr = Fix (Expression BinOp)


-- | Evaluate a `SimpleExpr` value.
evalSimpleExpr :: SimpleExpr -> Int
evalSimpleExpr (Fix expr) = evalExpr evalBinOp evalSimpleExpr expr


-- | Sample `SimpleExpr` value.
simple01 :: SimpleExpr
simple01 = Fix (ExprBinOp (Fix (ExprInt 5)) Add (Fix (ExprInt 5)))


--------------------------------------------------------------------------------
-- EXTENDED LANGUAGE representation instantiation, evaluation, and example
--
-- An extended base language representation can be instantiated by choosing
-- UniOp for unary operators, BinOpExt for binary operators, and similar
-- recursive expressions (done by Fix).
--
-- Evaluating the instantiated extended language representation only requires
-- using the predefined evaluation logic for the parameterized ADTs.
--------------------------------------------------------------------------------


-- | Instantiation of EXTENDED LANGUAGE representation.
type ExtendedExpr = Fix (ExpressionExt UniOp BinOpExt)


-- | Evaluate a `ExtendedExpr` value.
evalExtendedExpr :: ExtendedExpr -> Int
evalExtendedExpr (Fix expr) =
  evalExprExt evalUniOp evalBinOpExt evalExtendedExpr expr
--


-- | Sample `ExtendedExpr` value.
extended01 :: ExtendedExpr
extended01 = Fix (ExprUniOp Fac (Fix (ExprBase (ExprBinOp (Fix (ExprBase
  (ExprInt 3))) Mul (Fix (ExprBase (ExprInt 2)))))))


--------------------------------------------------------------------------------
-- Miscellaneous definitions
--------------------------------------------------------------------------------

-- | Type level `fix`.
data Fix f = Fix (f (Fix f))

-- | This is to make the compiler happy.
main :: IO ()
main = putStrLn "Hello World"
