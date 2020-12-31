{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeApplications #-}

module Experiment where

import Control.Monad (replicateM)
import Data.Coerce
import Graph
import Internal.Statistics
import Text.Printf
import Text.Tabular
import qualified Text.Tabular.AsciiArt as Ascii

--------------------------------------------------------------

data ProgramOpts = RunExperiment Config | EstimateK

data Config = Config {size :: Int, repetitions :: Int}

--------------------------------------------------------------

runExperiment :: Config -> IO (Statistics Weight)
runExperiment Config {..} = do
  r <- statistics <$> replicateM repetitions oneExperiment
  putStrLn (pretty r)
  return r
  where
    oneExperiment = do
      gr <- genGraph size
      let mst = prim gr
      return (totalWeight mst)

    pretty Statistics {..} =
      Ascii.render id id id table
      where
        table =
          Table
            ( Group
                NoLine
                [ Group NoLine [Header "E1"]
                ]
            )
            ( Group
                DoubleLine
                [ Group SingleLine [Header "|V|", Header "|E|", Header "W", Header "Std W"]
                ]
            )
            [ [show _n, show edges, show _mean, show _std]
            ]
        edges = (fromIntegral $ _n * (_n - 1)) / 2 :: Double

--------------------------------------------------------------

estimateK :: IO ()
estimateK = do
  -- The smaller the value the bigger the max weight
  let size = (16 :: Int) -- arbitrary, between 16 and 8196
  gr <- genGraph size
  let mst = prim gr
      w = (coerce $ maxWeight mst) :: Float
  printf "Maximum weight %.3f of MST\n" w
