{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Internal.Staticstics
  ( Staticstics (..),
    statistics,
  )
where

import Data.Foldable (foldl')

data Staticstics a = Staticstics
  { _n :: !Int,
    _mean :: !a,
    _variance :: !a,
    _std :: !a
  }
  deriving stock (Functor)

statistics :: (Foldable t, RealFrac a, Floating a) => t a -> Staticstics a
statistics = done . foldl' step begin
  where
    begin = (0, 0, 0)

    step (!n, !mean, !m2) x = (n', mean', m2')
      where
        n' = n + 1
        mean' = (n * mean + x) / (n + 1)
        delta = x - mean
        m2' = m2 + delta * delta * delta * n / (n + 1)

    done (n, mean, m2) = Staticstics (floor n) mean variance std
      where
        variance = m2 / n
        std = sqrt variance
{-# INLINEABLE statistics #-}
{-# SPECIALIZE statistics :: [Float] -> Staticstics Float #-}
{-# SPECIALIZE statistics :: [Double] -> Staticstics Double #-}
