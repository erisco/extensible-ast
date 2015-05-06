module Main where

-- Having a zillion type parameters in the extensible AST is not scalable
-- and is confusing, even if it does happen to work.
--

-- Another look at how this can be done...
-- We can project a concrete extended AST to a concrete simple AST.
-- Say we can then calculate the diff between two simple ASTs.
-- Say we can apply the diff of two simple ASTs to an extended AST.
-- Then: we project the extended AST to a simple AST and hand it off to the
--       simple AST optimiser. We can then compare the original projection
--       and the optimised version with a diff. Finally, the diff is applied
--       to our extended AST. Effectively, the simple AST optimiser has now
--       been reused to optimise our extended AST without crazy complication!
--       (Hopefully).
--
-- Additional idea: if calculating the diff between two ASTs is high cost,
--   it should be possible for the optimiser to construct a diff directly
--   instead.
--
-- project :: ExtendedExpr -> SimpleExpr
-- diff :: SimpleExpr -> SimpleExpr -> SimpleDiff
-- patch :: SimpleExpr -> SimpleDiff -> SimpleExpr
-- crossPatch :: ExtendedExpr -> SimpleDiff -> ExtendedExpr
--
-- crossPatch is where the secret sauce is. Also, how do we represent diffs?
--


main :: IO ()
main = undefined

-- This doesn't work, just back to recursive types being parameters
-- and once again not generic on binOp type.
{-
data ExprBin ex =
  ExprBin { exprBinLeft  :: ex
          , exprBinOp    :: BinOp
          , exprBinRight :: ex
          }
  deriving (Show)
--

-- Degenerates to Int
data ExprInt =
  ExprInt { exprInt :: Int }
  deriving (Show)
--

data BinOp = Add | Sub
  deriving (Show)
--

class ExpressionViewer ex where
  getExprBin :: ex -> Maybe (ExprBin ex)
  getExprInt :: ex -> Maybe Int
--
-}
