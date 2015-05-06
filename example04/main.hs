module Main where

-- import qualified Graph as G

import qualified Data.IntMap as IntMap
import Data.IntMap (IntMap)

import Control.Applicative
import Control.Monad

main :: IO ()
main = undefined

-- delete
-- insert
-- replace

data GraphEdit v =
    VertexDelete { graphDeleteIx :: Index }
  | VertexInsert { graphInsertIx     :: Index
                 , graphInsertVertex :: v
                 }
  | VertexReplace { graphReplaceTargetIx :: Index
                  , graphReplaceSourceIx :: Index
                  }
  deriving (Show)
--

-- Each edit involves one index, which is used to key this map.
-- This offers greater efficiency especially if you want to see what a graph
-- looks like without (permanently) applying the diff.
data GraphDiff v =
  GraphDiff { graphDiffEdits     :: IntMap (GraphEdit v)
            , graphDiffNextIndex :: Int
            }
  deriving (Show)
--

data Graph v =
  Graph { graphVertices  :: IntMap v
        , graphReverse   :: IntMap v
        , graphNextIndex :: Int
        }
  deriving (Show)
--

data DiffedGraph v =
  DiffedGraph { diffedGraphGraph :: Graph v
              , diffedGraphDiff  :: GraphDiff v
              }
  deriving (Show)
--

diffedGraphEmpty :: DiffedGraph v
diffedGraphEmpty = DiffedGraph graphEmpty graphDiffEmpty

graphEmpty :: Graph v
graphEmpty = Graph IntMap.empty IntMap.empty 0

graphDiffEmpty :: GraphDiff v
graphDiffEmpty = GraphDiff IntMap.empty 0

data GraphEditor v a =
  GraphEditor { graphEditorRun :: DiffedGraph v -> (DiffedGraph v, a) }
--

instance Functor (GraphEditor v) where
  fmap = liftA
--

instance Applicative (GraphEditor v) where
  pure = return
  (<*>) = ap
--

instance Monad (GraphEditor v) where
  return a = GraphEditor (\dg -> (dg, a))
  (GraphEditor f) >>= g = GraphEditor (\dg -> let (dg', x) = f dg
                                                  GraphEditor h = g x
                                               in h dg'
                                      )
--

-- TODO: needs cleanup
lookupVertex :: (Show v) => Index -> GraphEditor v v
lookupVertex ix =
  GraphEditor (\dg ->
    let diff  = graphDiffEdits . diffedGraphDiff $ dg
        graph = graphVertices . diffedGraphGraph $ dg
     in
       case IntMap.lookup ix diff of
        Just edit ->
          case edit of
            VertexDelete _ -> error $ "Vertex " ++ show ix
                                      ++ " was deleted from graph " ++ show graph
            VertexInsert _ vertex -> (dg, vertex)
            VertexReplace _ ix'   ->
              case IntMap.lookup ix' graph of
                Just vertex -> (dg, vertex)
                Nothing     -> error $ "Vertex (" ++ show ix'
                  ++ ") not found in graph, replaces vertex (" ++ show ix ++ ") "
                  ++ show graph
        Nothing ->
          case IntMap.lookup ix graph of
            Just vertex -> (dg, vertex)
            Nothing     -> error $ "Vertex " ++ show ix ++ " not found in graph "
                                    ++ show graph
  )
--

-- TODO: needs cleanup
{-
deleteVertex :: Index -> GraphEditor v ()
deleteVertex ix = GraphEditor (\dg ->
    let diff  = graphDiffEdits . diffedGraphDiff $ dg
        graph = graphVertices . diffedGraphGraph $ dg
     in GraphDiff { graphDiffEdits = IntMap.insert ix (VertexDelete ix) diff
                  , graphDiffNextIndex = ...
  )
-}

{-
class ExprNodeViewer a where
  data NodeValue = NodeValue { nodeIx :: Index, nodeValueValue :: Int }
  data NodeBinOp = NodeBinOp { nodeIx :: Index }
  getNodeValue :: a -> Maybe NodeValue
  getNodeBinOp :: a -> Maybe NodeBinOp
--
-}

type Index = Int

data ExprNode =
    NodeValue { nodeIx         :: Index
              , nodeValueValue :: Int
              }
  | NodeBinOp { nodeIx           :: Index
              , nodeBinOpLeftIx  :: Index
              , nodeBinOpOp      :: BinOp
              , nodeBinOpRightIx :: Index
              }
  deriving (Show)
--


data BinOp = Add | Sub
  deriving (Show)
--

data ExprGraph =
    ExprGraph { exprNodes     :: IntMap ExprNode
              , exprReverse   :: IntMap ExprNode
              , exprNextIndex :: Int
              }
  deriving (Show)
--

emptyExprGraph :: ExprGraph
emptyExprGraph = ExprGraph IntMap.empty IntMap.empty 0

nodeValue :: Int -> ExprGraph -> ExprGraph
nodeValue i g = g { exprNodes = n', exprReverse = r', exprNextIndex = ix' }
  where ix = exprNextIndex g
        ix' = ix + 1
        n' = IntMap.insert ix (NodeValue ix i) (exprNodes g)
        r' = exprReverse g
--

nodeBinOp :: Index -> BinOp -> Index -> ExprGraph -> ExprGraph
nodeBinOp l op r g = g { exprNodes = n', exprReverse = r', exprNextIndex = ix' }
  where ix = exprNextIndex g
        ix' = ix + 1
        n' = IntMap.insert ix binOpNode (exprNodes g)
        r' = IntMap.union (IntMap.fromList [(l, binOpNode), (r, binOpNode)])
                          (exprReverse g)
        binOpNode = NodeBinOp ix l op r
--

delete :: Index -> ExprGraph -> ExprGraph
delete i g = g { exprNodes = n', exprReverse = r', exprNextIndex = ix' }
  where n' = IntMap.delete i (exprNodes g)
        r' = IntMap.delete i (exprReverse g)
        ix' = exprNextIndex g
--

replace :: Index -> ExprNode -> ExprGraph -> ExprGraph
replace i n g = g { exprNodes = n', exprReverse = r', exprNextIndex = ix' }
  where n' = IntMap.update (const (Just n)) i (exprNodes g)
        r' = exprReverse g
        ix' = exprNextIndex g
--

findNode :: Index -> ExprGraph -> ExprNode
findNode i g = case IntMap.lookup i (exprNodes g) of
                 Just n  -> n
                 Nothing -> error "index not found in expression graph"
--

elimZeroAdd :: ExprNode -> ExprGraph -> ExprGraph
elimZeroAdd n g =
  case n of
    NodeBinOp _ ixL Add ixR ->
      case findNode ixL g of
        NodeValue _ 0 -> delete ixR . delete ixL . replace (nodeIx n) (findNode ixR g) $ g
        _ ->
          case findNode ixR g of
            NodeValue _ 0 -> delete ixL . delete ixR . replace (nodeIx n) (findNode ixL g) $ g
            _ -> g
    _ -> g
--

elimZeroAdd2 :: ExprNode -> ExprGraph -> IntMap (GraphEdit ExprNode)
elimZeroAdd2 n g =
  case n of
    NodeBinOp _ ixL Add ixR ->
      case findNode ixL g of
        NodeValue _ 0 -> IntMap.fromList
          [ (ixR, VertexDelete ixR)
          , (ixL, VertexDelete ixL)
          , (nodeIx n, VertexReplace (nodeIx n) ixR)
          ]
        _ ->
          case findNode ixR g of
            NodeValue _ 0 -> IntMap.fromList
              [ (ixR, VertexDelete ixR)
              , (ixL, VertexDelete ixL)
              , (nodeIx n, VertexReplace (nodeIx n) ixL)
              ]
            _ -> IntMap.empty
    _ -> IntMap.empty
--

example :: ExprGraph
example = nodeBinOp 0 Add 1 . nodeValue 0 . nodeValue 4 $ emptyExprGraph




{-
data ExprTree =
    ExprTree { getExprTreeGraph :: G.Graph
             , getExprTreeNode  :: IntMap ExprNode
             , getExprTreeRoot  :: Int
             , getExprTreeIndex :: Int
             }
  deriving (Show)
--

data ExprNode =
    ExprInt { getExprInt :: Int }
  | ExprBinOp { getExprBinOp :: BinOp }
  deriving (Show)
--

data BinOp = Add | Sub
  deriving (Show)
--

type ExprNodeIndex = Int


nextIndex :: ExprTree -> ExprNodeIndex
nextIndex = getExprTreeIndex

initial :: Int -> ExprTree
initial i = ExprTree
            (G.singleton (0, 1))
            (IntMap.singleton 1 (ExprInt i))
            1  -- root index
            2  -- next index
--

insertExprValue :: Int -> ExprTree -> ExprTree
insertExprValue i t = t { getExprTreeGraph = g'
                        , getExprTreeNode  = n'
                        , getExprTreeRoot  = getExprTreeRoot t
                        , getExprTreeIndex = nextIndex t + 1
                        }
  where g' = G.insert (0, nextIndex t) (getExprTreeGraph t)
        n' = IntMap.insert (nextIndex t) (ExprInt i) (getExprTreeNode t)
--

insertExprBinOp :: ExprNodeIndex -> BinOp -> ExprNodeIndex
                   -> ExprTree -> ExprTree
insertExprBinOp l op r t = t { getExprTreeGraph = g'
                             , getExprTreeNode  = n'
                             , getExprTreeRoot  = ix
                             , getExprTreeIndex = ix + 1
                             }
  where ix = nextIndex t
        g' = G.union (G.fromList [(0, ix), (ix, l), (ix, r)])
                     (getExprTreeGraph t)
        n' = IntMap.insert ix (ExprBinOp op) (getExprTreeNode t)
--

getRootNode :: ExprTree -> ExprNode
getRootNode t =
  case IntMap.lookup (getExprTreeRoot t) (getExprTreeNode t) of
    Just x  -> x
    Nothing -> error "root node does not exist in tree"
--

getExprBinOpOperands :: ExprNodeIndex -> ExprTree
                        -> (ExprNodeIndex, ExprNodeIndex)
getExprBinOpOperands i t =
--

elimZeroAdd :: ExprTree -> ExprTree
elimZeroAdd t =
  case getRootNode t of
    ExprBinOp Add ->
      let (l, r) = getExprBinOpOperands (getExprTreeRoot t) t
-}

{-
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
-}
