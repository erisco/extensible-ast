module ExtendedExprAbstract where

import qualified SimpleExprAbstract as SmplAbs
import ASTReader (ASTReader(ASTReader), (<?<), runASTReader)
import Control.Monad (guard)
import Data.Maybe (fromMaybe)


type Reader ex uniOp binOp a = ASTReader (Viewers ex uniOp binOp) a


data Viewers ex uniOp binOp =
  Viewers { getGetUniOp      :: ex -> Maybe (uniOp, ex)
          , getSimpleViewers :: SmplAbs.Viewers ex binOp
          }
--


getUniOp :: ex -> Reader ex uniOp binOp (uniOp, ex)
getUniOp ex = ASTReader (\s -> getGetUniOp s ex)

getExprBinOp :: ex -> Reader ex uniOp binOp (ex, binOp, ex)
getExprBinOp ex = ASTReader (\s -> SmplAbs.getGetExprBinOp (getSimpleViewers s) ex)

getExprInt :: ex -> Reader ex uniOp binOp Int
getExprInt ex = ASTReader (\s -> SmplAbs.getGetExprInt (getSimpleViewers s) ex)


data BinOp = Mul | Div | SimpleBinOp SmplAbs.BinOp
  deriving (Show, Eq)
--

data UniOp = Fac | Log

liftRdr :: SmplAbs.Reader ex binOp a -> Reader ex uniOp binOp a
liftRdr (ASTReader r) = ASTReader (r . getSimpleViewers)

elimSide ::   (ex -> ex -> ex)
           -> (Int -> binOp -> SmplAbs.Reader ex binOp Bool)
           -> ex
           -> Reader ex uniOp binOp ex
elimSide focal p ex = liftRdr $ SmplAbs.elimSide focal p ex

elimMulOne :: ex -> Reader ex uniOp binOp ex
elimMulOne = elimSide (\l _ -> l) p <?< elimSide (\r _ -> r) p
  where p i op = i == 1 && op == Mul
--

elimDivOne :: ex -> Reader ex uniOp binOp ex
elimDivOne = elimSide (\_ r -> r) (\i op -> i == 1 && op == Div)

optimise :: Viewers ex uniOp BinOp -> ex -> ex
optimise vs ex = fromMaybe ex $ runASTReader o vs
  where o = elimMulOne <?< elimDivOne $ SmplAbs.optimise (getSimpleViewers vs) ex
--
