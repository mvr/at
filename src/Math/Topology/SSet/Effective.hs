-- | A SSet with Effective Homology

module Math.Topology.SSet.Effective where

import Control.Category.Constrained
import Prelude hiding (id, (.))

import Math.Topology.SSet
import Math.Topology.SSet.NormalisedChains
import qualified Math.Algebra.ChainComplex as CC
import Math.Algebra.ChainComplex.Equivalence
import Math.Algebra.AbGroupPres

class (SSet a, CC.ChainComplex (Model a)) => Effective a where
  type Model a
  type Model a = NormalisedChains a

  model :: a -> Model a
  model = equivRight . eff

  eff :: a -> Equivalence (NormalisedChains a) (Model a)
  default eff :: (Model a ~ NormalisedChains a) => a -> Equivalence (NormalisedChains a) (Model a)
  eff = idEquiv . NormalisedChains

homology :: (Effective a, CC.FiniteType (Model a)) => a -> [AbGroupPres]
homology = CC.homologies . model
