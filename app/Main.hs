module Main where

import Experiment
import Options.Applicative

main :: IO ()
main = run =<< execParser opts
  where
    opts = info (programParser <**> helper) fullDesc

    run (RunExperiment config) = runExperiment config
    run EstimateK = estimateK

programParser :: Parser ProgramOpts
programParser = estimateKParser <|> (fmap RunExperiment configParser)

estimateKParser :: Parser ProgramOpts
estimateKParser =
  const EstimateK
    <$> switch
      ( long "estimate"
          <> short 'e'
          <> help "Estimate k(n)"
      )

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

