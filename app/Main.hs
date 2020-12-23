module Main where

import Experiment
import Options.Applicative

main :: IO ()
main = runExperiment =<< execParser opts
  where
    opts = info (configParser <**> helper) fullDesc

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

