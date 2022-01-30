{-# LANGUAGE UndecidableInstances #-}

-- | Normalised chain complex of a SSet
-- https://kerodon.net/tag/00QH
module Math.Topology.SSet.NormalisedChains where

import Control.Category.Constrained
import Data.Coerce
import Math.Algebra.ChainComplex as CC hiding (FiniteType, UMorphism (..))
import qualified Math.Algebra.ChainComplex as CC (FiniteType (..), UMorphism (..))
import Math.Topology.SSet
import Math.Topology.SSet.Morphism
import Prelude hiding (Functor, return)

newtype NormalisedChains a = NormalisedChains a
  deriving (Show)

newtype BasisSimplex a = BasisSimplex a
  deriving (Show, Eq)

instance SSet a => CC.ChainComplex (NormalisedChains a) where
  type Basis (NormalisedChains a) = BasisSimplex (GeomSimplex a)

  isBasis (NormalisedChains a) (BasisSimplex s) = isGeomSimplex a s

  degree (NormalisedChains a) = coerce $ geomSimplexDim a

  diff (NormalisedChains a) = coerce $ CC.Morphism (-1) act
    where
      act v = sum [Combination [(c, s)] | (c, NonDegen s) <- zip signs $ geomFaces a v]
      signs = cycle [1, -1]

instance FiniteType a => CC.FiniteType (NormalisedChains a) where
  dim (NormalisedChains a) i = length (geomBasis a i)
  basis (NormalisedChains a) i = coerce $ geomBasis a i

instance Functor UMorphism CC.UMorphism BasisSimplex where
  fmap m = CC.Morphism 0 $ \(BasisSimplex s) -> case m `onGeomSimplex` s of
    NonDegen t -> return $ BasisSimplex t
    Degen _ _ -> Combination []
