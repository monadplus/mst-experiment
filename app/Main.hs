{-# LANGUAGE TypeApplications #-}

module Main where

------------------------------------

import Data.Functor
import Experiment
import Options.Applicative

------------------------------------

main :: IO ()
main = run =<< execParser opts
  where
    opts = info (programParser <**> helper) fullDesc

    run (Run fp) = runExperiment fp
    run (SingleRun config) = void $ singleRun config
    run (EstimateK n) = estimateK n

programParser :: Parser ProgramOpts
programParser =
  subparser
    ( command "run" (info (runParser <**> helper) (progDesc "Run the experiment and plot the result."))
        <> command "single" (info (singleParser <**> helper) (progDesc "Single run on the given graph size."))
        <> command "estimate" (info (estimateKParser <**> helper) (progDesc "Estimate k(n) for the experiment."))
    )

runParser :: Parser ProgramOpts
runParser =
  Run
    <$> option
      auto
      ( long "file"
          <> short 'f'
          <> help "Plot output file"
          <> metavar "FILE"
      )

estimateKParser :: Parser ProgramOpts
estimateKParser =
  EstimateK
    <$> option
      auto
      ( long "size"
          <> short 'n'
          <> value 16
          <> showDefault
          <> help "Size of the complete undirected graph"
          <> metavar "INT"
      )

singleParser :: Parser ProgramOpts
singleParser = SingleRun <$> configParser

configParser :: Parser Config
configParser =
  Config
    <$> option
      auto
      ( long "size"
          <> short 'n'
          <> help "Size of the complete undirected graph"
          <> metavar "INT"
      )
    <*> option
      auto
      ( long "repetitions"
          <> short 'r'
          <> help "Number of repetitions of the experiment"
          <> value (5 :: Int)
          <> showDefault
          <> metavar "INT"
      )
