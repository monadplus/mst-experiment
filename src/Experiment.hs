{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeApplications #-}

module Experiment where

import Control.Monad (replicateM)
import Data.Semigroup
import Graph
import Text.Printf

data Config = Config
  { size :: Int,
    repetitions :: Int
  }

runExperiment :: Config -> IO ()
runExperiment Config {..} = do
  depth <- meanDepth <$> replicateM repetitions oneExperiment
  printf "Graph size %d\n" size
  printf "Depth %d\n" (unDepth depth)
  where
    oneExperiment :: IO Depth
    oneExperiment = do
      gr <- genGraph size
      let mst = prim gr
      return $ treeDepth mst

newtype Count a = Count (a, Int)
  deriving (Semigroup, Monoid) via ((Sum a, Sum Int))

newCount :: a -> Count a
newCount a = Count (a, 1)

getMean :: Fractional a => Count a -> a
getMean (Count (acc, n)) = acc / (fromIntegral n)

meanDepth :: [Depth] -> Depth
meanDepth = round @Double . getMean . foldMap (newCount . fromIntegral . unDepth)
