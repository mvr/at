-- | The simplicial circle with one vertex and one non-degenerate edge.
module Math.Topology.SSet.Circle where

import Math.Topology.SSet
import Math.Topology.SSet.Effective

data Circle = Circle
  deriving (Eq, Ord)

instance Show Circle where
  show _ = "S^1"

data CircleSimplex = CircleBasepoint | CircleEdge
  deriving (Eq, Ord, Show)

instance SSet Circle where
  type GeomSimplex Circle = CircleSimplex

  geomSimplexDim _ CircleBasepoint = 0
  geomSimplexDim _ CircleEdge = 1

  geomFace _ CircleBasepoint _ = undefined
  geomFace _ CircleEdge _ = NonDegen CircleBasepoint

instance FiniteType Circle where
  geomBasis _ 0 = [CircleBasepoint]
  geomBasis _ 1 = [CircleEdge]
  geomBasis _ _ = []

instance Pointed Circle where
  geomBasepoint _ = CircleBasepoint

instance ZeroReduced Circle

instance Effective Circle
