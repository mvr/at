{-# LANGUAGE UndecidableInstances #-}

-- | Shift of chain complexes
module Math.Algebra.ChainComplex.Shift where

import Math.Algebra.ChainComplex
import Prelude hiding (id, return, (.))

newtype Shift a = Shift a

instance (ChainComplex a) => ChainComplex (Shift a) where
  type Basis (Shift a) = Basis a
  isBasis (Shift a) = isBasis a
  degree (Shift a) s = degree a s + 1
  diff (Shift a) = Morphism (-1) go
    where
      go s = -(diff a `onBasis` s)

instance (FiniteType a) => FiniteType (Shift a) where
  dim (Shift a) n = dim a (n - 1)
  basis (Shift a) n = basis a (n - 1)
