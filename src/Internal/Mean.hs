{-# LANGUAGE DerivingVia #-}
module Internal.Mean where

new :: a -> Mean a
new a = Mean (a, 1)
{-# INLINE new #-}

data Statistics a = Statistics a
  { _n :: Int,
    _mean :: a,
    _variance :: a,
    _std :: a,
  }

getStatistics :: (Foldable t, Fractional a) => t a -> Statistics a
getStatistics
{-# INLINABLE getStatistics #-}
{-# SPECIALIZE getStatistics :: [Float] -> Float #-}

-- | Compute a numerically stable (population) variance over all elements
variance :: Fractional a => Fold a a
variance = Fold step begin done
  where
    begin = Pair3 0 0 0

    step (Pair3 n mean_ m2) x = Pair3 n' mean' m2'
      where
        n'     = n + 1
        mean'  = (n * mean_ + x) / (n + 1)
        delta  = x - mean_
        m2'    = m2 + delta * delta * n / (n + 1)

    done (Pair3 n _ m2) = m2 / n
{-# INLINABLE variance #-}
