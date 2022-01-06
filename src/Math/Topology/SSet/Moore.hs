-- | The Moore space M(Z/pZ, n) where p > 1 and n > 2p-4
module Math.Topology.SSet.Moore where

import Math.Topology.SSet
import Math.Topology.SSet.Effective

data Moore = Moore { mooreP :: Int, mooreDim :: Int }
  deriving (Eq, Ord, Show)

data MooreSimplex = Basepoint | N | NPlusOne
  deriving (Eq, Ord, Show)

instance SSet Moore where
  type GeomSimplex Moore = MooreSimplex

  isGeomSimplex _ _ = True

  geomSimplexDim (Moore p n) Basepoint = 0
  geomSimplexDim (Moore p n) N = n
  geomSimplexDim (Moore p n) NPlusOne = n + 1

  geomFace (Moore p n) Basepoint _ = undefined
  geomFace (Moore p n) N _ = constantAt Basepoint (n - 1)
  geomFace (Moore p n) NPlusOne i
    | even i && i < p*2 = NonDegen N
    | otherwise         = constantAt Basepoint n

instance LevelwiseFinite Moore where
  geomBasis (Moore p n) i
    | i == 0 = [Basepoint]
    | i == n = [N]
    | i == n + 1 = [NPlusOne]
    | otherwise = []

instance Pointed Moore where
  basepoint _ = Basepoint

instance Effective Moore
