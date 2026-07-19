{-# LANGUAGE TypeFamilies #-}

-- | Cellular chain complexes of disks.
module Math.Algebra.ChainComplex.Disk where

import Math.Algebra.ChainComplex
import Math.Algebra.ChainComplex.Reduction
import Math.Algebra.Combination

-- | @Disk n@ has a basepoint in degree 0, a boundary generator in
-- degree @n - 1@, and an interior generator in degree @n@. The
-- dimension must be positive.
newtype Disk = Disk Int
  deriving (Eq, Ord, Show)

data DiskBasis = DiskBase | DiskBoundary | DiskInterior
  deriving (Eq, Ord, Show)

instance ChainComplex Disk where
  type Basis Disk = DiskBasis

  isBasis (Disk n) _ = n > 0

  degree _ DiskBase = 0
  degree (Disk n) DiskBoundary = n - 1
  degree (Disk n) DiskInterior = n

  diff _ = Morphism (-1) $ \case
    DiskInterior -> singleComb DiskBoundary
    _ -> 0

instance FiniteType Disk where
  basis (Disk n) d
    | n <= 0 = []
    | d == 0 && n == 1 = [DiskBase, DiskBoundary]
    | d == 0 = [DiskBase]
    | d == n - 1 = [DiskBoundary]
    | d == n = [DiskInterior]
    | otherwise = []

-- | The canonical contraction of a disk onto its basepoint.
diskReduction :: Disk -> Reduction Disk ()
diskReduction _ = Reduction f g h
  where
    f = Morphism 0 $ \case
      DiskBase -> singleComb ()
      _ -> 0
    g = Morphism 0 (const (singleComb DiskBase))
    h = Morphism 1 $ \case
      DiskBoundary -> singleComb DiskInterior
      _ -> 0
