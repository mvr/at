-- | Shift of chain complexes
module Math.Algebra.ChainComplex.Shift where

import Math.Algebra.ChainComplex
import Math.Algebra.ChainComplex.Reduction
import Prelude hiding (id, return, (.))

-- | Shift a chain complex by an arbitrary number of degrees, so
-- @(Shift k C)_n = C_(n-k)@ with differential @(-1)^k d@.
data Shift a = Shift Int a

-- | Suspension by one degree.
newtype Susp a = Susp a

-- | Desuspension by one degree.
newtype Desusp a = Desusp a

instance (ChainComplex a) => ChainComplex (Shift a) where
  type Basis (Shift a) = Basis a
  isBasis (Shift _ a) = isBasis a
  degree (Shift k a) b = degree a b + k
  diff (Shift k a) = Morphism (-1) $ \b ->
    kozulRule k (diff a `onBasis` b)

instance (FiniteType a) => FiniteType (Shift a) where
  dim (Shift k a) n = dim a (n - k)
  basis (Shift k a) n = basis a (n - k)

instance ChainComplex a => ChainComplex (Susp a) where
  type Basis (Susp a) = Basis a
  isBasis (Susp a) = isBasis (Shift 1 a)
  degree (Susp a) = degree (Shift 1 a)
  diff (Susp a) = sameBasisMorphism (diff (Shift 1 a))

instance FiniteType a => FiniteType (Susp a) where
  dim (Susp a) = dim (Shift 1 a)
  basis (Susp a) = basis (Shift 1 a)

instance ChainComplex a => ChainComplex (Desusp a) where
  type Basis (Desusp a) = Basis a
  isBasis (Desusp a) = isBasis (Shift (-1) a)
  degree (Desusp a) = degree (Shift (-1) a)
  diff (Desusp a) = sameBasisMorphism (diff (Shift (-1) a))

instance FiniteType a => FiniteType (Desusp a) where
  dim (Desusp a) = dim (Shift (-1) a)
  basis (Desusp a) = basis (Shift (-1) a)

shiftReduction ::
  (ChainComplex a, ChainComplex b) =>
  Int ->
  Reduction a b ->
  Reduction (Shift a) (Shift b)
shiftReduction k (Reduction f g h) =
  Reduction
    (sameBasisMorphism f)
    (sameBasisMorphism g)
    (kozulRule k (sameBasisMorphism h))

suspReduction ::
  (ChainComplex a, ChainComplex b) =>
  Reduction a b ->
  Reduction (Susp a) (Susp b)
suspReduction r = sameBasisReduction (shiftReduction 1 r)

desuspReduction ::
  (ChainComplex a, ChainComplex b) =>
  Reduction a b ->
  Reduction (Desusp a) (Desusp b)
desuspReduction r = sameBasisReduction (shiftReduction (-1) r)
