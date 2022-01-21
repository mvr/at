{-# LANGUAGE UndecidableInstances #-}

-- | Twisted product of chain complexes of free Z-modules. The left
-- factor must be a coalgebra and the right an algebra. There is an
-- (unimplemented) extension to comodules and modules.
-- See Section 8.3 in https://arxiv.org/abs/1208.3816
-- Twisting cochains and power maps in https://arxiv.org/abs/1106.4787
-- Anything useful in https://arxiv.org/abs/1006.2781 on algebra structures?
module Math.Algebra.ChainComplex.TwistedTensor where

import Control.Category.Constrained
import Data.Coerce
import Math.Algebra.ChainComplex
import Math.Algebra.ChainComplex.Algebra
import Math.Algebra.ChainComplex.Coalgebra
import Math.Algebra.ChainComplex.Reduction
import Math.Algebra.ChainComplex.Tensor
import Prelude hiding (id, return, (.))

-- | The twisting cochain tau has degree -1
data TwistedTensor a b = TwistedTensor a b (Morphism a b)

newtype TwistedBasis a = TwistedBasis a
  deriving (Eq)

perturbationForCochain :: (Coalgebra a, Algebra b, Eq (Basis a), Eq (Basis b)) => a -> b -> Morphism a b -> Morphism (Tensor a b) (Tensor a b)
perturbationForCochain a b tauMor = delta
  where
    (ClosedMorphism _ delta _) = (idA ⊗ mu) . assoc . ((idA ⊗ tau) ⊗ idB) . (del ⊗ idB)

    mu = ClosedMorphism (Tensor b b) (muMor b) b
    del = ClosedMorphism a (delMor a) (Tensor a a)
    tau = ClosedMorphism a tauMor b
    idA = ClosedMorphism a id a
    idB = ClosedMorphism b id b
    assoc = ClosedMorphism (Tensor (Tensor a b) b) tensorAssoc (Tensor a (Tensor b b))
    (⊗) = tensorMorphismArr

-- | Calculating the twisting cochain from a perturbation on a tensor
-- product: franz:twisting
-- C(B) η⊗1 −−→ C(G) ⊗ C(B) dt−d⊗ −−−→ C(G) ⊗ C(B) 1⊗ε −−→ C(G)
cochainForPerturbation :: (Coalgebra a, Algebra b, Eq (Basis a), Eq (Basis b)) => a -> b -> Morphism (Tensor a b) (Tensor a b) -> Morphism a b
cochainForPerturbation a b delta = undefined

instance (Coalgebra a, Algebra b, Eq (Basis b), Eq (Basis a)) => ChainComplex (TwistedTensor a b) where
  type Basis (TwistedTensor a b) = TwistedBasis (Basis (Tensor a b))

  isBasis (TwistedTensor a b _) (TwistedBasis (s, t)) = isBasis a s && isBasis b t
  degree (TwistedTensor a b _) (TwistedBasis s) = degree (Tensor a b) s

  diff (TwistedTensor a b tauMor) = coerce $ diff (Perturbed (Tensor a b) (perturbationForCochain a b tauMor))

toTwisted :: (Coalgebra a, Algebra b, Eq (Basis a), Eq (Basis b)) => Perturbed (Tensor a b) -> TwistedTensor a b
toTwisted (Perturbed (Tensor a b) delta) = TwistedTensor a b (cochainForPerturbation a b delta)

fromTwisted :: (Coalgebra a, Algebra b, Eq (Basis a), Eq (Basis b)) => TwistedTensor a b -> Perturbed (Tensor a b)
fromTwisted (TwistedTensor a b tau) = Perturbed (Tensor a b) (perturbationForCochain a b tau)

isoPerturbed :: forall a b. Morphism (Perturbed (Tensor a b)) (TwistedTensor a b)
isoPerturbed = Morphism 0 (coerce @((Basis a, Basis b) -> Combination _) return)

isoPerturbedInv :: forall a b. Morphism (TwistedTensor a b) (Perturbed (Tensor a b))
isoPerturbedInv = Morphism 0 (coerce @((Basis a, Basis b) -> Combination _) return)
