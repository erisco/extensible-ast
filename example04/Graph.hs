{-# LANGUAGE UnboxedTuples, ScopedTypeVariables #-}
module Graph where

import Prelude hiding (lookup)

import qualified Data.IntMap.Strict as IntMap
import qualified Data.IntMap.Strict as IndexMap
import Data.IntMap.Strict (IntMap)

import qualified Data.Vector.Unboxed as Vector
import Data.Vector.Unboxed (Vector, Unbox)

import qualified Data.Vector.Unboxed.Mutable as MVector
import Data.Vector.Unboxed.Mutable (MVector)

import Control.Monad.Primitive (PrimMonad, PrimState)

import Control.Monad.ST

-- TODO: index width should be parameterizable
type Index = Int

type IndexMap a = IntMap a

data Vertex a =
  Vertex { vertexEdgesOut :: {-# UNPACK #-} !(Vector Index)
         , vertexLabel    :: {-# UNPACK #-} !a
         , vertexEdgesIn  :: {-# UNPACK #-} !(Vector Index)
         }
  deriving (Show)
--

instance Functor Vertex where
  fmap f (Vertex out l inn) = Vertex out (f l) inn
--

-- Parametric label type.
-- Vertices are stored as (label, vertex index at other end of edge).
--   User can rely on out edge vector order (but not in edges).
-- Unique indices are assigned to every vertex.
data Graph a =
  Graph { graphVertices  :: {-# UNPACK #-} !(IndexMap (Vertex a))
        , graphNextIndex :: {-# UNPACK #-} !Index
        }
  deriving (Show)
--

instance Functor Graph where
  fmap f g = g { graphVertices = fmap (fmap f) (graphVertices g) }
--

empty :: Graph a
empty = Graph { graphVertices  = IndexMap.empty
              , graphNextIndex = minBound :: Int
              }
--

-- TODO: convert to unboxed tuple
singleton :: a -> (Graph a, Index)
singleton l = (g, ix)
  where v = Vertex { vertexEdgesIn  = Vector.empty
                   , vertexEdgesOut = Vector.empty
                   , vertexLabel    = l
                   }
        g = Graph { graphVertices  = IndexMap.fromList [(ix, v)]
                  , graphNextIndex = ix + 1
                  }
        ix = minBound :: Int
--

-- TODO: convert to unboxed tuple (does not work in GHCi, need conditional
--                                 compilation blocks)
insert :: Vertex a -> Graph a -> (Graph a, Index)
insert v g = (g', ix)
  where ix = graphNextIndex g
        g' = Graph { graphVertices  = IndexMap.insert ix v (graphVertices g)
                   , graphNextIndex = ix + 1
                   }
--

-- TODO: have to delete edges too
delete :: Index -> Graph a -> Graph a
delete ix g = g { graphVertices = IndexMap.delete ix (graphVertices g) }

-- Source vertex is copied into target vertex. In edges are aggregated.
supplant :: Index -> Index -> Graph a -> Graph a
supplant tIx sIx g = g { graphVertices = vs' }
  where vs' = IndexMap.insert tIx v (graphVertices g)
        v = Vertex { vertexEdgesIn  = in'
                   , vertexEdgesOut = out'
                   , vertexLabel = vertexLabel s
                   }
        in'  = removeDuplicates $ vertexEdgesIn t Vector.++ vertexEdgesIn s
        out' = vertexEdgesOut s
        t = lookup tIx g
        s = lookup sIx g
--

-- TODO: cleanup
removeDuplicates :: (Unbox a, Ord a) => Vector a -> Vector a
removeDuplicates v = runST $ do
  let sorted = insertionSort v
      length_v = Vector.length v
  v' <- MVector.new 3 -- TODO: find correct length
  (i, _) <- for
    (\(i, j) -> i < length_v - 2)
    (\(i, j) -> do let a = sorted Vector.! i
                       b = sorted Vector.! (i + 1)
                   if a == b
                   then return (i + 1, j)
                   else do MVector.unsafeWrite v' j a
                           return (i + 1, j + 1)
    )
    (0, 0)
  MVector.unsafeWrite v' 2 (sorted Vector.! i) -- TODO: use correct length
  Vector.unsafeFreeze v'
--

-- Uses a loop counter rather than, say, mapping over some [Int]
for_ :: (Monad m, Ord a, Num a) => a -> a -> (a -> m ()) -> m ()
for_ min max f = loop min
  where loop i
          | i < max   = f i >> for_ (i + 1) max f
          | otherwise = return ()
--

for :: (Monad m) => (s -> Bool) -> (s -> m s) -> s -> m s
for p b s = loop s
  where loop s
          | p s       = b s >>= loop
          | otherwise = return s
--

-- should insertionSort take/return MVector instead?
-- TODO: cleanup. Note that annotations were needed to make the thing compile
insertionSort :: forall a. (Unbox a, Ord a) => Vector a -> Vector a
insertionSort immut_v = runST (mut_v >>= sort 0)

  where mut_v :: forall s. ST s (MVector (PrimState (ST s)) a)
        mut_v = Vector.thaw immut_v

        length_v = Vector.length immut_v

        sort i v
          | i < length_v = do vi <- MVector.unsafeRead v i
                              insert (i - 1) vi v
                              sort (i + 1) v
          | otherwise    = Vector.unsafeFreeze v

        insert i x v
          | i < 0     = MVector.unsafeWrite v 0 x
          | otherwise = do vi <- MVector.unsafeRead v i
                           if x < vi
                           then do MVector.unsafeWrite v (i + 1) vi
                                   insert (i - 1) x v
                           else MVector.unsafeWrite v (i + 1) x
--

lookup :: Index -> Graph a -> Vertex a
lookup ix g = case IndexMap.lookup ix (graphVertices g) of
                Just v  -> v
                Nothing -> error $ "Vertex " ++ show ix ++ " not found in graph"
--

example :: (PrimMonad m) => m (MVector (PrimState m) Int)
example = Vector.thaw . Vector.fromList $ [4,7,2,1]
