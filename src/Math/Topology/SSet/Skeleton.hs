{-# LANGUAGE UndecidableInstances #-}

-- |
module Math.Topology.SSet.Skeleton where

import Prelude hiding (Bounded)

import Math.Topology.SSet

data Skeleton a = Skeleton Int a
  deriving (ZeroReduced) via a
  deriving (OneReduced) via a

instance Show a => Show (Skeleton a) where
  show (Skeleton n a) = "sk_" ++ show n ++ " " ++ show a

newtype SkeletonSimplex a = SkeletonSimplex a
  deriving (Eq, Ord, Show) via a

instance SSet a => SSet (Skeleton a) where
  type GeomSimplex (Skeleton a) = SkeletonSimplex (GeomSimplex a)

  isGeomSimplex (Skeleton n a) (SkeletonSimplex s) = geomSimplexDim a s <= n && isGeomSimplex a s

  geomSimplexDim (Skeleton _ a) (SkeletonSimplex s) = geomSimplexDim a s

  geomFace (Skeleton _ a) (SkeletonSimplex s) i = SkeletonSimplex <$> geomFace a s i

instance FiniteType a => FiniteType (Skeleton a) where
  geomBasis (Skeleton n a) i
    | i <= n = SkeletonSimplex <$> geomBasis a i
    | otherwise = []

-- `a` shouldn't have to be bounded here, but replacing `amplitude`
-- with `[0..n]` actually might make things worse rather than better
instance Bounded a => Bounded (Skeleton a) where
  amplitude (Skeleton n a) = filter (<= n) (amplitude a)

instance Pointed a => Pointed (Skeleton a) where
  basepoint (Skeleton _ a) = SkeletonSimplex (basepoint a)

-- instance Effective (Skeleton a)
