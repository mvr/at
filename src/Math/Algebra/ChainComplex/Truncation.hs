-- | Degreewise truncation of chain complexes.
module Math.Algebra.ChainComplex.Truncation where

import Math.Algebra.ChainComplex
import Math.Algebra.Combination

-- | The naive truncation below degree @n@. It agrees with the original
-- complex in degrees at least @n@, vanishes below @n@, and sets the
-- differential out of degree @n@ to zero.
--
-- Unlike good truncation, this can create homology in the cutoff degree.
data NaiveTruncation a = NaiveTruncation Int a

instance ChainComplex a => ChainComplex (NaiveTruncation a) where
  type Basis (NaiveTruncation a) = Basis a

  isBasis (NaiveTruncation n a) b =
    degree a b >= n && isBasis a b
  degree (NaiveTruncation _ a) = degree a
  diff (NaiveTruncation n a) = Morphism (-1) $ \b ->
    if degree a b == n
      then zeroCombination
      else diff a `onBasis` b

instance ChainComplex a => BoundedBelow (NaiveTruncation a) where
  lowerBound (NaiveTruncation n _) = n

instance FiniteType a => FiniteType (NaiveTruncation a) where
  dim (NaiveTruncation n a) d
    | d < n = 0
    | otherwise = dim a d

  basis (NaiveTruncation n a) d
    | d < n = []
    | otherwise = basis a d
