{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE NoStarIsType #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

module Internal.Graph
  ( -- * Graph
    UGraph,
    mkUGraph,
    genGraph,

    -- * Tree
    LTree,
    type Tree,

    -- * Minimum Spanning Tree
    MST,
    Weight (..),
    Depth (..),
    prim,
    prim',
    treeDepth,
    maxWeight,
    totalWeight,

    -- * Iso
    Iso,
    type (#),
  )
where

import Control.DeepSeq
import Control.Monad.IO.Class
import Control.Monad.ST (ST)
import Data.Coerce
import qualified Data.Graph.Inductive as G
import Data.Graph.Inductive.PatriciaTree (Gr)
import Data.Kind
import Data.Semigroup (Max (..), Sum (..))
import qualified System.Random.MWC as R
import Graphics.Rendering.Chart.Easy (PlotValue)

-- | Given \( n \), returns a complete undirected graph with \( n \) vertices and \( \binom{n}{2} \) edges.
newtype UGraph = UGraph {unGraph :: Gr () Weight}
  deriving newtype (NFData)

type family Iso (a :: Type) :: Type where
  Iso (a, ()) = a
  Iso (a, b) = (a, b)

type (#) a label = Iso (a, label)

newtype LTree label = LTree {unLTree :: [[Int # label]]}

deriving newtype instance Show (Iso (Int, label)) => Show (LTree label)

deriving newtype instance NFData (Iso (Int, label)) => NFData (LTree label)

type Tree = LTree ()

{-
>>> print mst
[[(1,0.0)],
 [(9,9.083688e-3),(1,0.0)],
 [(10,0.24761155),(9,9.083688e-3),(1,0.0)],
 [(3,0.24981353),(1,0.0)],
 [(7,1.0946065e-2),(3,0.24981353),(1,0.0)],
 [(8,0.21356645),(7,1.0946065e-2),(3,0.24981353),(1,0.0)],
 [(4,0.26288742),(8,0.21356645),(7,1.0946065e-2),(3,0.24981353),(1,0.0)],
 [(5,2.5864244e-2),(4,0.26288742),(8,0.21356645),(7,1.0946065e-2),(3,0.24981353),(1,0.0)],
 [(2,0.20782933),(4,0.26288742),(8,0.21356645),(7,1.0946065e-2),(3,0.24981353),(1,0.0)],
 [(6,0.34417707),(8,0.21356645),(7,1.0946065e-2),(3,0.24981353),(1,0.0)]]
-}
newtype MST = MST {unTree :: [[(Int, Weight)]]}
  deriving newtype (Show, NFData)

newtype Weight = Weight {unWeight :: Float}
  deriving newtype (Show, Ord, Eq, Num, Enum, Real, RealFrac, Fractional, Floating, RealFloat, NFData, PlotValue)
  deriving (Semigroup, Monoid) via (Sum Float)

instance R.Variate Weight where
  uniform = fmap Weight . R.uniform
  uniformR (Weight w1, Weight w2) = fmap Weight . R.uniformR (w1, w2)

newtype Depth = Depth {unDepth :: Int}
  deriving newtype (Show, Ord, Eq, Num, Enum, Real, Integral)

-- | From a directed to undirected graph.
--
-- Note, it has to create all missing backwards edges.
mkUGraph :: Gr () Weight -> UGraph
mkUGraph = UGraph . G.undir
{-# INLINE mkUGraph #-}

type Node = G.LNode ()

mkNode :: Int -> Node
mkNode = (,())

type Edge = G.LEdge Weight

mkEdge :: Weight -> Node -> Node -> Edge
mkEdge weight (u, _) (v, _) = (u, v, weight)
{-# INLINE mkEdge #-}

genGraph :: (MonadIO m) => Int -> m UGraph
genGraph n = liftIO $ R.withSystemRandom . R.asGenST $ genGraph'
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
prim :: UGraph -> MST
prim (UGraph gr) = coerce $ G.msTree gr

prim' :: UGraph -> Tree
prim' = coerce . (fmap . fmap) (\(node, _label) -> node) . unTree . prim

treeDepth :: MST -> Depth
treeDepth = coerce @(Max Int) . foldMap (coerce . length) . coerce @MST @[[Int # Weight]]

-- | Returns the edge with maximum weight of a MST
maxWeight :: MST -> Weight
maxWeight = maximum . fmap snd . concat . coerce @MST @[[Int # Weight]]

-- | Returns the sum of weights of a MST
totalWeight :: MST -> Weight
totalWeight =
  -- Each list contains a branch in ascending order from node to root.
  -- The first element is the new node.
  foldMap (snd . head) . coerce @MST @[[Int # Weight]]
