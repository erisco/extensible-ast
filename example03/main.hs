{-# LANGUAGE GADTs, FlexibleContexts #-}
module Main where

import Prelude hiding (id, (.))
import Control.Monad
import Data.Maybe (fromMaybe)
import Control.Category
import Control.Applicative

data Fix f = Fix (f (Fix f))


-- Apply these optimising rewrites:
-- 0 + x = x
-- x + 0 = x
-- x - 0 = x
-- x / 1 = x
-- x * 1 = x
-- 1 * x = x

{-
data Reader s a b = Reader { runReader :: s -> a -> b }

instance Category (Reader s) where
  id = Reader (\_ -> id)
  (Reader g) . (Reader f) = Reader (\s a -> g s (f s a))
--

instance Arrow (Reader s) where
  arr f = Reader (\_ -> f)
  first (Reader f) = Reader (\s (a, b) -> (f s a, b))
--

data SimpleGetters ex op =
  SimpleGetters { getGetExprBinOp :: ex -> Maybe (ex, op, ex)
                , getGetExprInt   :: ex -> Maybe Int
                }
--

type SimpleRewriter ex op = Reader (SimpleGetters ex op) ex ex

--elimLeft2 :: (Int -> op -> Bool) -> SimpleRewriter ex op
elimLeft2 p = proc expr -> do
  x <- Reader getGetExprBinOp -< expr
  returnA -< id
-}

-- NOTE: attempting to rewrite using a Maybe + State monad so that the getters
--       for the AST do not have to be explicit parameters (there will be many
--       getters for complicated languages)
-- NOTE : the name "Rewriter" does not quite make sense, since a rewriter would
--        then be   ex -> Rewriter s ex

data CommonGetters ex op =
  CommonGetters { getGetExprBinOp :: ex -> Maybe (ex, op, ex)
                , getGetExprInt   :: ex -> Maybe Int
                }
--

getExprBinOp :: ex -> Rewriter (CommonGetters ex op) (ex, op, ex)
getExprBinOp ex = Rewriter (\s -> getGetExprBinOp s ex)

getExprInt :: ex -> Rewriter (CommonGetters ex op) Int
getExprInt ex = Rewriter (\s -> getGetExprInt s ex)


data Rewriter s a = Rewriter { runRewriter :: s -> Maybe a }

get :: Rewriter s s
get = Rewriter (Just . id)


instance Functor (Rewriter s) where
  fmap = liftM
--

instance Applicative (Rewriter s) where
  pure  = return
  (<*>) = ap
--

instance Alternative (Rewriter s) where
  empty = Rewriter (\_ -> Nothing)
  (Rewriter f) <|> (Rewriter g) = Rewriter (\s -> f s <|> g s)
--

instance Monad (Rewriter s) where
  return a = Rewriter (\_ -> Just a)
  (Rewriter f) >>= rg = Rewriter h
    where h s = case f s of
                  Just x  -> runRewriter (rg x) s
                  Nothing -> Nothing
--

instance MonadPlus (Rewriter s) where
  mzero = empty
  mplus = (<|>)
--


type CommonRewriter ex op a = Rewriter (CommonGetters ex op) a

elimSide ::   (ex -> ex -> ex)
           -> (Int -> op -> Bool)
           -> ex
           -> CommonRewriter ex op ex
elimSide focal p ex =  do
  (l, op, r) <- getExprBinOp ex
  i <- getExprInt (focal l r)
  guard (p i op)
  return (focal r l)
--

(>|<) :: (Alternative f) => (a -> f b) -> (a -> f b) -> a -> f b
(>|<) = liftA2 (<|>)

attempt :: (Alternative f) => (a -> f a) -> a -> f a
attempt = flip (>|<) pure

(<?<) :: (Monad m, Alternative m) => (a -> m a) -> (a -> m a) -> a -> m a
(<?<) g f = attempt g <=< attempt f



elimZeroAdd :: ex -> CommonRewriter ex BinOp ex
elimZeroAdd = elimSide (\l _ -> l) p <?< elimSide (\_ r -> r) p
  where p i op = i == 0 && op == Add
--

elimZeroSub :: ex -> CommonRewriter ex BinOp ex
elimZeroSub = elimSide (\_ r -> r) (\i op -> i == 0 && op == Sub)

optimise :: SimpleExpr -> SimpleExpr
optimise ex = fromMaybe ex $ runRewriter ((elimZeroSub <?< elimZeroAdd) ex) simpleGetters

deepOptimise :: SimpleExpr -> SimpleExpr
deepOptimise ex =
  case ex of
    Fix (ExprBinOp l op r) -> optimise $ Fix (ExprBinOp (optimise l) op (optimise r))
    x -> x
--


{-
type Elim ex op =
     (ex -> Maybe (ex, op, ex))
  -> (ex -> Maybe Int)
  -> ex
  -> ex
--

elimLeft :: (Int -> op -> Bool) -> Elim ex op
elimLeft p getExprBinOp' getExprInt' expr = fromMaybe expr $ do
  (l, op, r) <- getExprBinOp' expr
  i <- getExprInt' l
  guard (p i op)
  return r
--

elimRight :: (op -> Int -> Bool) -> Elim ex op
elimRight p = elimLeft (flip p) . flipGetExprBinOp
  where
    flipGetExprBinOp :: (e -> Maybe (e, op, e)) -> e -> Maybe (e, op, e)
    flipGetExprBinOp = (fmap (\(l, op, r) -> (r, op, l)) .)
--

elimZeroAdd :: Elim ex BinOp
elimZeroAdd g1 g2 = elimLeft elim g1 g2 . elimRight (flip elim) g1 g2
  where elim i o = i == 0 && o == Add
--

elimZeroSub :: Elim ex BinOp
elimZeroSub = elimRight (\o i -> i == 0 && o == Sub)



getExprBinOp :: (Fix (Expression op) ~ e) => e -> Maybe (e, op, e)
getExprBinOp (Fix (ExprBinOp l op r)) = Just (l, op, r)
getExprBinOp _                        = Nothing

getExprInt :: (Fix (Expression op) ~ e) => e -> Maybe Int
getExprInt (Fix (ExprInt i)) = Just i
getExprInt _                 = Nothing


optimise :: SimpleExpr -> SimpleExpr
optimise = ctx elimZeroAdd . ctx elimZeroSub
  where ctx f = f getExprBinOp getExprInt
--


deepOptimise :: SimpleExpr -> SimpleExpr
deepOptimise (Fix (ExprBinOp l op r)) = o
  where l' = deepOptimise l
        r' = deepOptimise r
        o  = optimise (Fix (ExprBinOp l' op r'))
deepOptimise e = e
-}

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

data BinOp = Add | Sub deriving (Show, Eq)


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



simBinOp :: SimpleExpr -> BinOp -> SimpleExpr -> SimpleExpr
simBinOp l op r = Fix (ExprBinOp l op r)

simInt :: Int -> SimpleExpr
simInt i = Fix (ExprInt i)

simAdd :: SimpleExpr -> SimpleExpr -> SimpleExpr
simAdd l r = simBinOp l Add r

simSub :: SimpleExpr -> SimpleExpr -> SimpleExpr
simSub l r = simBinOp l Sub r


printSimpleExpr :: SimpleExpr -> String
printSimpleExpr (Fix (ExprBinOp l op r))
  = "(" ++ printSimpleExpr l ++ " " ++ show op ++ " " ++ printSimpleExpr r ++ ")"
printSimpleExpr (Fix (ExprInt i)) = show i


simpleGetters :: CommonGetters SimpleExpr BinOp
simpleGetters = CommonGetters
  { getGetExprInt   = (\ex -> case ex of Fix (ExprInt i) -> Just i; _ -> Nothing)
  , getGetExprBinOp = (\ex -> case ex of Fix (ExprBinOp l op r) -> Just (l, op, r); _ -> Nothing)
  }
--

simple01 :: SimpleExpr
simple01 = (simInt 0 `simAdd` simInt 5) `simAdd` (simInt 0 `simSub` simInt 0)


--
-- Instantiation of the extended AST.
--

type ExtendedExpr = Fix (ExpressionExt UniOp BinOpExt)


extended01 :: ExtendedExpr
extended01 = Fix (ExprUniOp Fac (Fix (ExprBase (ExprBinOp (Fix (ExprBase (ExprInt 3))) Mul (Fix (ExprBase (ExprInt 2)))))))


-- This is to make the compiler happy.

main :: IO ()
main = putStrLn "Hello World"
