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
import Data.Bifunctor
import Text.Tabular
import qualified Text.Tabular.AsciiArt as Ascii

--------------------------------------------------------------

data ProgramOpts = RunExperiment Config | EstimateK

data Config = Config {size :: Int, repetitions :: Int}

--------------------------------------------------------------

runExperiment :: Config -> IO (Depth, Weight)
runExperiment Config {..} = do
  info <- meanDepth <$> replicateM repetitions oneExperiment
  putStrLn (pretty info)
  return info
  where
    oneExperiment :: IO (Depth, Weight)
    oneExperiment = do
      gr <- genGraph size
      let mst = prim gr
      return (treeDepth mst, totalWeight mst)

    meanDepth :: [(Depth, Weight)] -> (Depth, Weight)
    meanDepth xs = bimap (round @Double . Mean.getMean) (Mean.getMean) $ foldMap toMonoid xs
      where toMonoid (depth, weight) = (Mean.new (fromIntegral depth), Mean.new weight)

    pretty :: (Depth, Weight) -> String
    pretty (depth, weight) =
      Ascii.render id id id table
        where table =  Table
                (Group NoLine
                  [ Group NoLine [Header "E1"]
                  ])
                (Group DoubleLine
                  [ Group SingleLine [Header "|V|", Header "|E|", Header "depth", Header "sum of weight"]
                  ])
                [[show size, show edges, show depth, show weight]
                ]
              edges = (fromIntegral $ size*(size - 1))/2 :: Double

--------------------------------------------------------------

estimateK :: IO ()
estimateK = do
  -- The smaller the value the bigger the max weight
  let size = (16 :: Int) -- arbitrary, between 16 and 8196
  gr <- genGraph size
  let mst = prim gr
      w = (coerce $ maxWeight mst) :: Float
  printf "Maximum weight %.3f of MST\n" w
