{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeApplications #-}

module Experiment where

import Control.Monad (replicateM)
import Data.Coerce
import Graph
import Text.Printf
import qualified Internal.Mean as Mean

--------------------------------------------------------------

data ProgramOpts = RunExperiment Config | EstimateK

data Config = Config {size :: Int, repetitions :: Int}

--------------------------------------------------------------

runExperiment :: Config -> IO ()
runExperiment Config {..} = do
  depth <- meanDepth <$> replicateM repetitions oneExperiment
  printf "Graph size %d\n" size
  printf "Depth %d\n" (unDepth depth)
  where
    oneExperiment :: IO Depth
    oneExperiment = do
      gr <- genGraph size
      let mst = prim' gr
      return $ treeDepth mst

meanDepth :: [Depth] -> Depth
meanDepth = round @Double . Mean.getMean . foldMap (Mean.new . fromIntegral . unDepth)

--------------------------------------------------------------

estimateK :: IO ()
estimateK = do
  -- The smaller the value the bigger the max weight
  let size = (16 :: Int) -- arbitrary, between 16 and 8196
  gr <- genGraph size
  let mst = prim gr
      w = (coerce $ maxWeight mst) :: Float
  printf "Maximum weight %.3f of MST\n" w
