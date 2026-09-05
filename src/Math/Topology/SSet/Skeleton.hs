module Math.Topology.SSet.Skeleton where

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

instance Pointed a => Pointed (Skeleton a) where
  geomBasepoint (Skeleton _ a) = SkeletonSimplex (geomBasepoint a)

-- instance Effective (Skeleton a)
