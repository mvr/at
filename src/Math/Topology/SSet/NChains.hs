-- | Normalised chain complex of a SSet
-- See, eg. <https://kerodon.net/tag/00QH>
module Math.Topology.SSet.NChains where

import Control.Category.Constrained
import Prelude hiding (Functor, return)

import Math.Algebra.ChainComplex as CC hiding (FiniteType, Morphism (..))
import qualified Math.Algebra.ChainComplex as CC (FiniteType (..), Morphism (..))
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

instance SSet a => CC.BoundedBelow (NChains a) where
  lowerBound _ = 0

instance ZeroReduced a => CC.ConnectedChainComplex (NChains a)

instance OneReduced a => CC.OneReducedChainComplex (NChains a)

-- | Regard a simplex as an element of the normalised chain complex.
-- Degenerate simplices represent zero.
asChain :: FormalDegen a -> Combination a
asChain (NonDegen simplex) = singleComb simplex
asChain (Degen _ _) = zeroCombination

instance Functor Morphism CC.Morphism NChains where
  fmap m = CC.Morphism 0 $ \s -> asChain (m `onGeomSimplex` s)
