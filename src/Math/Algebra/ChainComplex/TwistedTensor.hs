{-# LANGUAGE UndecidableInstances #-}

-- | Twisted product of chain complexes of free Z-modules. The left
-- factor must be a coalgebra and the right an algebra. There is an
-- (unimplemented) extension to comodules and modules.
-- See Section 8.3 in https://arxiv.org/abs/1208.3816
module Math.Algebra.ChainComplex.TwistedTensor where

import Control.Category.Constrained
import Data.Coerce
import Math.Algebra.ChainComplex
import Math.Algebra.ChainComplex.Algebra
import Math.Algebra.ChainComplex.Coalgebra
import Math.Algebra.ChainComplex.Reduction
import Math.Algebra.ChainComplex.Tensor
import Prelude hiding (id, (.))

-- | The twisting cochain tau has degree -1
data TwistedTensor a b = TwistedTensor a b (Morphism a b)

instance (Coalgebra a, Algebra b, Eq (Basis b), Eq (Basis a)) => ChainComplex (TwistedTensor a b) where
  type Basis (TwistedTensor a b) = Basis (Tensor a b)

  isBasis (TwistedTensor a b _) (s, t) = isBasis a s && isBasis b t
  degree (TwistedTensor a b _) s = degree (Tensor a b) s

  diff (TwistedTensor a b tauMor) = coerce $ diff (Perturbed (Tensor a b) delta)
    where
      (ClosedMorphism _ delta _) = (idA ⊗ mu) . assoc . ((idA ⊗ tau) ⊗ idB) . (del ⊗ idB)
      mu = ClosedMorphism (Tensor b b) (muMor b) b
      del = ClosedMorphism a (delMor a) (Tensor a a)
      tau = ClosedMorphism a tauMor b
      idA = ClosedMorphism a id a
      idB = ClosedMorphism b id b
      assoc = ClosedMorphism (Tensor (Tensor a b) b) (tensorAssoc a b b) (Tensor a (Tensor b b))
      (⊗) = tensorMorphismArr
