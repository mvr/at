{-# LANGUAGE UndecidableInstances #-}

-- | Real projective space \(ℝP^n\)
module Math.Topology.SSet.RPn where

import Data.Coerce
import Prelude hiding (Bounded)

import Math.Topology.SGrp.KGn
import Math.Topology.SSet
import Math.Topology.SSet.Effective
import Math.Topology.SSet.Skeleton

newtype RPn = RPn Int

instance Show RPn where
  show (RPn n) = "ℝP^" ++ show n

newtype RPnSimplex = RPnSimplex (GeomSimplex (Skeleton KZmod2_1))
  deriving (Eq, Ord)
  deriving (Show) via GeomSimplex (Skeleton KZmod2_1)

instance SSet RPn where
  type GeomSimplex RPn = RPnSimplex
  isGeomSimplex (RPn n) = coerce (isGeomSimplex (Skeleton n KZmod2_1))
  geomSimplexDim (RPn n) = coerce (geomSimplexDim (Skeleton n KZmod2_1))
  geomFace (RPn n) = coerce (geomFace (Skeleton n KZmod2_1))

instance FiniteType RPn where
  geomBasis (RPn n) = coerce (geomBasis (Skeleton n KZmod2_1))

instance Bounded RPn where
  amplitude (RPn n) = [0 .. n]

instance Pointed RPn where
  basepoint (RPn n) = coerce (basepoint KZmod2_1)

instance ZeroReduced RPn
