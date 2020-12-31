{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Internal.Statistics
  ( Statistics (..),
    statistics,
  )
where

import Data.Foldable (foldl')

data Statistics a = Statistics
  { _n :: !Int,
    _mean :: !a,
    _variance :: !a,
    _std :: !a
  }
  deriving stock (Functor)

statistics :: (Foldable t, RealFrac a, Floating a) => t a -> Statistics a
statistics = done . foldl' step begin
  where
    begin = (0, 0, 0)

    step (!n, !mean, !m2) x = (n', mean', m2')
      where
        n' = n + 1
        mean' = (n * mean + x) / (n + 1)
        delta = x - mean
        m2' = m2 + (delta * delta) * n / (n + 1)

    done (n, mean, m2) = Statistics (floor n) mean variance std
      where
        variance = m2 / n
        std = sqrt variance
{-# INLINEABLE statistics #-}
{-# SPECIALIZE statistics :: [Float] -> Statistics Float #-}
{-# SPECIALIZE statistics :: [Double] -> Statistics Double #-}
