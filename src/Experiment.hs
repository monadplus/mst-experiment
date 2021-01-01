{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE NamedFieldPuns #-}

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
    repetitions :: Int,
    outputFile :: FilePath
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
runExperiment fp = plot fp =<< results
  where
    outputFile = (baseName fp ++ ".txt")
    results =
      traverse
        singleRun
        [ Config{ size = 16, repetitions = 20, outputFile}
        , Config{ size = 32, repetitions = 20, outputFile}
        , Config{ size = 64, repetitions = 20, outputFile}
        , Config{ size = 128, repetitions = 20, outputFile}
        , Config{ size = 256, repetitions = 10, outputFile}
        , Config{ size = 512, repetitions = 5, outputFile}
        , Config{ size = 1024, repetitions = 5, outputFile}
        , Config{ size = 2048, repetitions = 2, outputFile}
        , Config{ size = 4096, repetitions = 2, outputFile}
        -- beyond this point it takes too long..
        ]

--------------------------------------------------------------

-- | Single run of the experiment.
singleRun :: Config -> IO (Int, Statistics Weight)
singleRun Config {..} = do
  r <- statistics <$> replicateM repetitions oneExperiment
  let str = pretty r
  appendFile (baseName outputFile ++ ".txt") str
  putStrLn str
  return (size, r)
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
            [ [show size, show edges, show _mean, show _std]
            ]
        edges = (fromIntegral $ size * (size - 1)) / 2 :: Double

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
