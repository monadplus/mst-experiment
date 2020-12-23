{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE NoStarIsType #-}

module Graph where

import Control.Monad.IO.Class
import Control.Monad.ST (ST)
import Data.Coerce
import qualified Data.Graph.Inductive as G
import Data.Graph.Inductive.PatriciaTree (Gr)
import Data.Kind
import Data.Semigroup (Max (..))
import qualified System.Random.MWC as R

-- | Given \( n \), returns a complete undirected graph with \( n \) vertices and \( \binom{n}{2} \) edges.
newtype UGraph = UGraph {unGraph :: Gr () Weight}

type family Iso (a :: Type) :: Type where
  Iso (a, ()) = a
  Iso (a, b) = (a, b)

type (#) a label = Iso (a, label)

newtype LTree label = LTree {unLTree :: [[Int # label]]}

type Tree = LTree ()

newtype Weight = Weight {unWeight :: Float}
  deriving newtype (Show, Ord, Eq, Num, Enum, Real, Fractional)

instance R.Variate Weight where
  uniform = fmap Weight . R.uniform
  uniformR (Weight w1, Weight w2) = fmap Weight . R.uniformR (w1, w2)

newtype Depth = Depth {unDepth :: Int}
  deriving newtype (Show, Ord, Eq, Num, Enum, Real, Integral)

mkUGraph :: Gr () Weight -> UGraph
mkUGraph = UGraph . G.undir -- undir is expensive

type Node = G.LNode ()

mkNode :: Int -> Node
mkNode = (,())

type Edge = G.LEdge Weight

mkEdge :: Weight -> Node -> Node -> Edge
mkEdge weight (u, _) (v, _) = (u, v, weight)

genGraph :: (MonadIO m) => Int -> m UGraph
genGraph n = liftIO $ R.withSystemRandomST genGraph'
  where
    genGraph' :: forall s. R.Gen s -> ST s UGraph
    genGraph' gen = do
      let vertices = fmap mkNode [1 .. n]
      edges <- filterEdges <$> genEdges [] vertices
      return $ mkUGraph (G.mkGraph vertices edges)
      where
        genEdges :: [Edge] -> [Node] -> ST s [Edge]
        genEdges acc [] = return acc
        genEdges acc (v : vs) = do
          newEdges <- v `connectTo` vs
          genEdges (acc ++ newEdges) vs
        connectTo :: Node -> [Node] -> ST s [Edge]
        connectTo u = traverse $ \v -> do
          weight <- R.uniform @Weight gen
          return $ mkEdge weight u v
        -- In order to reduce the memory footprint of the graph on large n, we take advantage of
        -- the observation that the minimum spanning tree only contains edges of weight < k(n) by pruning
        -- the edges with weight > k(n). The tricky part is computing k(n) which can be estimated running
        -- some simulations on different sizes of graphs.
        filterEdges :: [Edge] -> [Edge]
        filterEdges
          | n <= 512 = id
          | otherwise = filter (\(_, _, w) -> w < k n)
          where
            -- On n=512 ==> max weight = 0.011
            -- On n=1024 ==> max weight = 0.007
            -- ...
            k = const 0.1

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
prim :: UGraph -> LTree Weight
prim (UGraph gr) = coerce $ G.msTree gr

prim' :: UGraph -> Tree
prim' = LTree . (fmap . fmap) (\(node, _label) -> node) . unLTree . prim

treeDepth :: forall label. LTree label -> Depth
treeDepth (LTree tree) =
  coerce @(Max Int) $ foldMap (coerce . length) tree

-- | Returns the edge with maximum weight
maxWeight :: LTree Weight -> Weight
maxWeight = maximum . fmap snd . concat . unLTree
