{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}

module Graph where

import Control.Monad.IO.Class
import Control.Monad.ST (ST)
import Data.Coerce
import qualified Data.Graph.Inductive as G
import Data.Graph.Inductive.PatriciaTree (Gr)
import Data.Semigroup (Max (..))
import qualified System.Random.MWC as R

-- | Given \( n \), returns a complete undirected graph with \( n \) vertices and \( \binom{n}{2} \) edges.
newtype UGraph = UGraph {unGraph :: Gr () Float}

newtype Tree = Tree {unTree :: [[Int]]}

newtype Depth = Depth {unDepth :: Int}
  deriving newtype (Show, Ord, Eq, Num, Enum, Real, Integral)

-- Note this adds an overhead of undirecting the graph.
mkUGraph :: Gr () Float -> UGraph
mkUGraph = UGraph . G.undir

type Node = G.LNode ()

mkNode :: Int -> Node
mkNode = (,())

type Edge = G.LEdge Float

mkEdge :: Float -> Node -> Node -> Edge
mkEdge weight (u, _) (v, _) = (u, v, weight)

genGraph :: (MonadIO m) => Int -> m UGraph
genGraph n = liftIO $ R.withSystemRandomST genGraph'
  where
    genGraph' :: forall s. R.Gen s -> ST s UGraph
    genGraph' gen = do
      let vertices = fmap mkNode [1 .. n]
      edges <- genEdges [] vertices
      return $ mkUGraph (G.mkGraph vertices edges)
      where
        genEdges :: [Edge] -> [Node] -> ST s [Edge]
        genEdges acc [] = return acc
        genEdges acc (v : vs) = do
          newEdges <- v `connectTo` vs
          genEdges (acc ++ newEdges) vs
        connectTo :: Node -> [Node] -> ST s [Edge]
        connectTo u = traverse $ \v -> do
          weight <- R.uniform gen
          return $ mkEdge weight u v

-- | Prim's Algorithm
--
-- @
-- newEdges :: LPath b -> Context a b -> [H.Heap b (LPath b)]
-- newEdges (LP p) (_,_,_,s) = map (\(l,v)->H.unit l (LP ((v,l):p))) s
--
-- prim :: (Graph gr,Real b) => H.Heap b (LPath b) -> gr a b -> LRTree b
-- prim h g | H.isEmpty h || isEmpty g = []
-- prim h g =
--    case match v g of
--         (Just c,g')  -> p:prim (H.mergeAll (h':newEdges p c)) g'
--         (Nothing,g') -> prim h' g'
--    where (_,p@(LP ((v,_):_)),h') = H.splitMin h
--
-- msTreeAt :: (Graph gr,Real b) => Node -> gr a b -> LRTree b
-- msTreeAt v = prim (H.unit 0 (LP [(v,0)]))
--
-- msTree :: (Graph gr,Real b) => gr a b -> LRTree b
-- msTree g = msTreeAt v g where ((_,v,_,_),_) = matchAny g
-- @
--
-- An output example: @[[1],[4,1],[3,4,1],[2,1],[5,4,1]]@
prim :: UGraph -> Tree
prim = Tree . fmap (fmap fst . G.unLPath) . G.msTree . unGraph

treeDepth :: Tree -> Depth
treeDepth (Tree tree) =
  coerce @(Max Int) $ foldMap (coerce . length) tree

pretty :: UGraph -> String
pretty = G.prettify . unGraph
