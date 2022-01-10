{-# LANGUAGE UndecidableInstances #-}

-- | Normalised chain complex of a SSet
-- https://kerodon.net/tag/00QH
module Math.Topology.NormalisedChains where

import Data.Coerce
import qualified Math.Algebra.ChainComplex as CC (FiniteType(..))
import Math.Algebra.ChainComplex as CC hiding (FiniteType)
import Math.Algebra.ChainComplex.Coalgebra
import Math.Topology.SSet

newtype NormalisedChains a = NormalisedChains a
  deriving (Show)
newtype BasisSimplex a = BasisSimplex a
  deriving (Show, Eq)

instance (Eq (GeomSimplex a), SSet a) => CC.ChainComplex (NormalisedChains a) where
  type Basis (NormalisedChains a) = BasisSimplex (GeomSimplex a)
  degree (NormalisedChains a) = coerce $ geomSimplexDim a
  diff (NormalisedChains a) = coerce $ Morphism (-1) act
    where
      act v = sum [Combination [(c, s)] | (c, NonDegen s) <- zip signs $ geomFaces a v]
      signs = cycle [1, -1]

instance (FiniteType a, Eq (GeomSimplex a)) => CC.FiniteType (NormalisedChains a) where
  dim (NormalisedChains a) i = length (geomBasis a i)
  basis (NormalisedChains a) i = coerce $ geomBasis a i

instance (SSet a, Eq (GeomSimplex a)) => Coalgebra (NormalisedChains a) where
  counitMor (NormalisedChains a) = undefined -- 1 on each 0-simplex
  delMor (NormalisedChains a) = Morphism 0 undefined -- todo: define in terms of AW map
