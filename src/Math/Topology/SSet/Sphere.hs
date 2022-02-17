-- | The \(n\)-sphere \(S^n\)
module Math.Topology.SSet.Sphere where

import Prelude hiding (Bounded)

import Math.Topology.SSet
import Math.Topology.SSet.Effective

newtype Sphere = Sphere { sphereDim :: Int }

instance Show Sphere where
  show (Sphere n) = "S^" ++ show n

data SphereSimplex = Basepoint | Cell
  deriving (Eq, Ord, Show)

instance SSet Sphere where
  type GeomSimplex Sphere = SphereSimplex

  isGeomSimplex _ _ = True

  geomSimplexDim (Sphere n) Basepoint = 0
  geomSimplexDim (Sphere n) Cell = n

  geomFace (Sphere n) Basepoint _ = undefined
  geomFace (Sphere n) Cell _ = constantAt Basepoint (n - 1)
  geomFaces (Sphere n) Basepoint = []
  geomFaces (Sphere n) Cell = replicate (n+1) $ constantAt Basepoint (n - 1)

instance FiniteType Sphere where
  geomBasis (Sphere n) i
    | i == 0 = [Basepoint]
    | i == n = [Cell]
    | otherwise = []

instance Bounded Sphere where
  amplitude (Sphere n) = [0, n]

instance Pointed Sphere where
  basepoint _ = Basepoint

instance ZeroReduced Sphere

-- err, as long as d > 1
instance OneReduced Sphere

instance Effective Sphere
