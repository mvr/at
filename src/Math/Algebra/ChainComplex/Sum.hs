{-# LANGUAGE UndecidableInstances #-}

-- | Direct sum of chain complexes
module Math.Algebra.ChainComplex.Sum where

import Math.Algebra.ChainComplex
import Prelude hiding (id, return, (.))

data Sum a b = Sum a b

instance (ChainComplex a, ChainComplex b) => ChainComplex (Sum a b) where
  type Basis (Sum a b) = Either (Basis a) (Basis b)
  isBasis (Sum a b) (Left s) = isBasis a s
  isBasis (Sum a b) (Right t) = isBasis b t
  degree (Sum a b) (Left s) = degree a s
  degree (Sum a b) (Right t) = degree b t
  diff (Sum a b) = Morphism (-1) go
    where
      go (Left s) = Left <$> (diff a `onBasis` s)
      go (Right t) = Right <$> (diff b `onBasis` t)

instance (FiniteType a, FiniteType b) => FiniteType (Sum a b) where
  dim (Sum a b) n = dim a n + dim b n
  basis (Sum a b) n = (Left <$> basis a n) ++ (Right <$> basis b n)
