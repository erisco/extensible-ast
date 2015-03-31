module SimpleExprAbstract where

import ASTReader (ASTReader(ASTReader), (<?<), runASTReader)
import Control.Monad (guard)
import Data.Maybe (fromMaybe)

-- Note: not doing this because down the road is the diamond problem (from
--       inheritance).
{-
type Reader vs ex op a = ASTReader (Viewers vs ex op) a


data Viewers vs ex op =
  Viewers { getGetExprBinOp :: ex -> Maybe (ex, op, ex)
          , getGetExprInt   :: ex -> Maybe Int
          , getGetBinOp     :: op -> Maybe BinOp
          , getOtherViewers :: vs
          }
--
-}

type Reader ex op a = ASTReader (Viewers ex op) a


data Viewers ex op =
  Viewers { getGetExprBinOp :: ex -> Maybe (ex, op, ex)
          , getGetExprInt   :: ex -> Maybe Int
          , getGetBinOp     :: op -> Maybe BinOp
          }
--


getExprBinOp :: ex -> Reader ex op (ex, op, ex)
getExprBinOp ex = ASTReader (\s -> getGetExprBinOp s ex)

getExprInt :: ex -> Reader ex op Int
getExprInt ex = ASTReader (\s -> getGetExprInt s ex)

getBinOp :: op -> Reader ex op BinOp
getBinOp op = ASTReader (\s -> getGetBinOp s op)


data BinOp = Add | Sub
  deriving (Show, Eq)
--


elimSide ::   (ex -> ex -> ex)
           -> (Int -> op -> Reader ex op Bool)
           -> ex
           -> Reader ex op ex
elimSide focal p ex =  do
  (l, op, r) <- getExprBinOp ex
  i <- getExprInt (focal l r)
  guard =<< p i op
  return (focal r l)
--

elimZeroAdd :: ex -> Reader ex op ex
elimZeroAdd = elimSide (\l _ -> l) p <?< elimSide (\_ r -> r) p
  where p i op = do
          binOp <- getBinOp op
          return $ i == 0 && binOp == Add
--

elimZeroSub :: ex -> Reader ex op ex
elimZeroSub = elimSide (\_ r -> r) p
  where p i op = do
          binOp <- getBinOp op
          return $ i == 0 && binOp == Sub
--

optimise :: Viewers ex op -> ex -> ex
optimise vs ex = fromMaybe ex $ runASTReader o vs
  where o = elimZeroSub <?< elimZeroAdd $ ex
--
