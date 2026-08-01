{-# LANGUAGE UndecidableInstances #-}

-- | Normalised chain complex of a SSet
-- See, eg. <https://kerodon.net/tag/00QH>
module Math.Topology.SSet.NChains where

import Control.Category.Constrained
import Prelude hiding (Bounded, Functor, return)

import Math.Algebra.ChainComplex as CC hiding (Bounded, FiniteType, Morphism (..), amplitude)
import qualified Math.Algebra.ChainComplex as CC (Bounded, FiniteType (..), Morphism (..), amplitude)
import Math.Algebra.Combination
import Math.Topology.SSet

-- | Normalised chain complex of a `SSet`
newtype NChains a = NChains a

instance Show a => Show (NChains a) where
  show (NChains a) = "N(" ++ show a ++ ")"

instance SSet a => CC.ChainComplex (NChains a) where
  type Basis (NChains a) = GeomSimplex a

  isBasis (NChains a) = isGeomSimplex a

  degree (NChains a) = geomSimplexDim a

  diff (NChains a) = CC.Morphism (-1) act
    where
      act v = fromTerms [(sign i, s) | (i, s) <- geomNonDegenFaces a v]
      sign i = if even i then 1 else -1

instance FiniteType a => CC.FiniteType (NChains a) where
  dim (NChains a) i = length (geomBasis a i)
  basis (NChains a) = geomBasis a

instance Bounded a => CC.Bounded (NChains a) where
  amplitude (NChains a) = amplitude a

instance ZeroReduced a => CC.ConnectedChainComplex (NChains a)

instance OneReduced a => CC.OneReducedChainComplex (NChains a)

instance Functor Morphism CC.Morphism NChains where
  fmap m = CC.Morphism 0 $ \s -> case m `onGeomSimplex` s of
    NonDegen t -> singleComb t
    Degen _ _ -> zeroCombination
