{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeApplications #-}

module Experiment
  ( Config (..),
    ProgramOpts (..),
    runExperiment,
    singleRun,
    estimateK
  )
where

--------------------------------------------------------------

import Control.Monad (replicateM)
import Data.Coerce
import Internal.Graph
import Internal.Plot
import Internal.Statistics
import Text.Printf
import Text.Tabular
import qualified Text.Tabular.AsciiArt as Ascii

--------------------------------------------------------------

data Config = Config
  { size :: Int,
    repetitions :: Int
  }
  deriving stock (Show)

data ProgramOpts
  = Run FilePath
  | SingleRun Config
  | EstimateK Int
  deriving stock (Show)

--------------------------------------------------------------

-- | Experiment to estimate the upper bound on the weight
-- of the MST of a complete undirected graph.
runExperiment :: FilePath -> IO ()
runExperiment fp = plot fp []

--------------------------------------------------------------

-- | Single run of the experiment.
singleRun :: Config -> IO (Statistics Weight)
singleRun Config {..} = do
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

-- | Estimation of the function /K(n)/ to reduce the complexity of the experiment.
--
-- Note, this function is inverse linear to the number of vertices.
estimateK :: Int -> IO ()
estimateK size = do
  gr <- genGraph size
  let mst = prim gr
      w = (coerce $ maxWeight mst) :: Float
  printf "Maximum weight %.3f of MST\n" w
