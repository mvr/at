-- | A SSet with attached Effective Homology

module Math.Topology.SSet.Effective where

import Control.Category.Constrained
import Prelude hiding (id, (.))

import Math.Topology.SSet
import Math.Topology.NormalisedChains
import qualified Math.Algebra.ChainComplex as CC
import Math.Algebra.ChainComplex.Equivalence
import Math.Algebra.AbGroupPres

class (SSet a, CC.ChainComplex (Model a)) => Effective a where
  type Model a
  type Model a = NormalisedChains a

  model :: a -> Model a
  eff :: a -> Equivalence (NormalisedChains a) (Model a)

  default model :: (Model a ~ NormalisedChains a) => a -> Model a
  model = NormalisedChains
  default eff :: (Model a ~ NormalisedChains a, Eq (GeomSimplex a)) => a -> Equivalence (NormalisedChains a) (Model a)
  eff _ = id

homology :: (Effective a, Eq (CC.Basis (Model a)), CC.FiniteType (Model a)) => a -> [AbGroupPres]
homology = CC.homologies . model
