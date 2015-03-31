module ASTReader
  ( ASTReader(ASTReader), runASTReader, get, (>|<), (<?<), (>?>), attempt
  ) where
--

import           Control.Applicative (Alternative, Applicative, empty, liftA2,
                                      pure, (<*>), (<|>))
import           Control.Monad       (MonadPlus, ap, liftM, mplus, mzero, (<=<))

data ASTReader s a = ASTReader { runASTReader :: s -> Maybe a }

instance Functor (ASTReader s) where
  fmap = liftM
--

instance Applicative (ASTReader s) where
  pure  = return
  (<*>) = ap
--

instance Alternative (ASTReader s) where
  empty = ASTReader (const Nothing)
  ASTReader f <|> ASTReader g = ASTReader (\s -> f s <|> g s)
--

instance Monad (ASTReader s) where
  return a = ASTReader (\_ -> Just a)
  ASTReader f >>= rg = ASTReader h
    where h s = case f s of
                  Just x  -> runASTReader (rg x) s
                  Nothing -> Nothing
--

instance MonadPlus (ASTReader s) where
  mzero = empty
  mplus = (<|>)
--

get :: ASTReader s s
get = ASTReader Just

(>|<) :: (Alternative f) => (a -> f b) -> (a -> f b) -> a -> f b
(>|<) = liftA2 (<|>)

attempt :: (Alternative f) => (a -> f a) -> a -> f a
attempt = flip (>|<) pure

(<?<) :: (Monad m, Alternative m) => (a -> m a) -> (a -> m a) -> a -> m a
(<?<) g f = attempt g <=< attempt f

(>?>) :: (Monad m, Alternative m) => (a -> m a) -> (a -> m a) -> a -> m a
(>?>) = flip (<?<)
