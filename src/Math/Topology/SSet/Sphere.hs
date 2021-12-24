-- | The n-sphere S^n
module Math.Topology.SSet.Sphere where

import Math.Topology.SSet

newtype Sphere = Sphere { sphereDim :: Int }
  deriving (Eq, Ord, Show)

data SphereSimplex = Basepoint | Cell
  deriving (Eq, Ord, Show)

instance SSet Sphere where
  type GeomSimplex Sphere = SphereSimplex

  isSimplex _ _ = True

  simplexDim (Sphere n) Basepoint = 0
  simplexDim (Sphere n) Cell = n

  geomFace (Sphere n) Basepoint _ = undefined
  geomFace (Sphere n) Cell _ = constantAtVertex (Sphere n) Basepoint (n - 1)

instance LevelwiseFinite Sphere where
  geomBasis (Sphere n) i
    | i == 0 = [Basepoint]
    | i == n = [Cell]
    | otherwise = []
