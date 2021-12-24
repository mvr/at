-- | The Moore space M(Z/pZ, n) where p > 1 and n > 2p-4
module Math.Topology.SSet.Moore where

import Math.Topology.SSet

data Moore = Moore { mooreP :: Int, mooreDim :: Int }
  deriving (Eq, Ord, Show)

data MooreSimplex = Basepoint | N | NPlusOne
  deriving (Eq, Ord, Show)

instance SSet Moore where
  type GeomSimplex Moore = MooreSimplex

  isSimplex _ _ = True

  simplexDim (Moore p n) Basepoint = 0
  simplexDim (Moore p n) N = n
  simplexDim (Moore p n) NPlusOne = n+1

  geomFace (Moore p n) Basepoint _ = undefined
  geomFace (Moore p n) N _ = constantAtVertex (Moore p n) Basepoint (n - 1)
  geomFace (Moore p n) NPlusOne i
    | even i && i < p*2 = NonDegen N
    | otherwise         = constantAtVertex (Moore p n) Basepoint n

instance LevelwiseFinite Moore where
  geomBasis (Moore p n) i
    | i == 0 = [Basepoint]
    | i == n = [N]
    | i == n + 1 = [NPlusOne]
    | otherwise = []
