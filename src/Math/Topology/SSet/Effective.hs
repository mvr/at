-- | A SSet with Effective Homology

module Math.Topology.SSet.Effective where

import Control.Category.Constrained
import Prelude hiding (id, (.))

import Math.Topology.SSet
import Math.Topology.SSet.NChains
import qualified Math.Algebra.ChainComplex as CC
import Math.Algebra.ChainComplex.Equivalence
import Math.Algebra.AbGroupPres

class (SSet a, CC.ChainComplex (Model a)) => Effective a where
  type Model a
  type Model a = NChains a

  model :: a -> Model a
  model = equivRight . eff

  eff :: a -> Equivalence (NChains a) (Model a)
  default eff :: (Model a ~ NChains a) => a -> Equivalence (NChains a) (Model a)
  eff = idEquiv . NChains

homology :: (Effective a, CC.FiniteType (Model a)) => a -> [AbGroupPres]
homology = CC.homologies . model
