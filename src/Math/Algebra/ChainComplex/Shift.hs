{-# LANGUAGE UndecidableInstances #-}

-- | Shift of chain complexes
module Math.Algebra.ChainComplex.Shift where

import Math.Algebra.ChainComplex
import Prelude hiding (id, return, (.))

newtype Shift a = Shift a
newtype ShiftBasis a = ShiftBasis a
  deriving (Eq, Show)

instance (ChainComplex a) => ChainComplex (Shift a) where
  type Basis (Shift a) = ShiftBasis (Basis a)
  isBasis (Shift a) (ShiftBasis s) = isBasis a s
  degree (Shift a) (ShiftBasis s) = degree a s + 1
  diff (Shift a) = Morphism (-1) go
    where
      go (ShiftBasis s) = ShiftBasis <$> (diff a `onBasis` s)

instance (FiniteType a) => FiniteType (Shift a) where
  dim (Shift a) n = dim a (n - 1)
  basis (Shift a) n = ShiftBasis <$> basis a (n - 1)
