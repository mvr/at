{-# LANGUAGE UndecidableInstances #-}

-- | Normalised chain complex of a SSet
-- https://kerodon.net/tag/00QH
module Math.Topology.NormalisedChains where

import qualified Math.Algebra.ChainComplex as CC (LevelwiseFinite(..))
import Math.Algebra.ChainComplex as CC hiding (LevelwiseFinite)
import Math.Algebra.ChainComplex.Coalgebra
import Math.Topology.SSet

newtype NormalisedChains a = NormalisedChains a
  deriving (Show)

instance (Eq (GeomSimplex a), SSet a) => CC.ChainComplex (NormalisedChains a) where
  type Basis (NormalisedChains a) = GeomSimplex a
  degree (NormalisedChains a) = geomSimplexDim a
  diff (NormalisedChains a) = Morphism (-1) act
    where
      act v = sum [Combination [(c, s)] | (c, NonDegen s) <- zip signs $ geomFaces a v]
      signs = cycle [1, -1]

instance (LevelwiseFinite a, Eq (GeomSimplex a)) => CC.LevelwiseFinite (NormalisedChains a) where
  dim (NormalisedChains a) i = length (geomBasis a i)
  basis (NormalisedChains a) i = geomBasis a i

instance (SSet a, Eq (GeomSimplex a)) => Coalgebra (NormalisedChains a) where
  counitMor (NormalisedChains a) = undefined -- 1 on each 0-simplex
  delMor (NormalisedChains a) = Morphism 0 undefined -- todo: define in terms of AW map
