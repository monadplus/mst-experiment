{-# LANGUAGE DerivingVia #-}
module Internal.Mean where

import Data.Semigroup

newtype Mean a = Mean (a, Int)
  deriving (Semigroup, Monoid) via ((Sum a, Sum Int))

new :: a -> Mean a
new a = Mean (a, 1)

getMean :: Fractional a => Mean a -> a
getMean (Mean (acc, n)) = acc / (fromIntegral n)
