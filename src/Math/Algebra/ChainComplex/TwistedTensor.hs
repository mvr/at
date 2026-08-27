-- | Twisted products of chain complexes of free Z-modules, in algebra-first
-- order for a right action.
-- See Section 8.3 in https://arxiv.org/abs/1208.3816
-- Twisting cochains and power maps in https://arxiv.org/abs/1106.4787
-- Anything useful in https://arxiv.org/abs/1006.2781 on algebra structures?
module Math.Algebra.ChainComplex.TwistedTensor where

import Control.Category.Constrained
import Math.Algebra.ChainComplex
import Math.Algebra.ChainComplex.Algebra
import Math.Algebra.ChainComplex.Coalgebra
import Math.Algebra.ChainComplex.Reduction
import Math.Algebra.ChainComplex.Tensor
import Prelude hiding (id, return, (.))

-- | A twisted tensor product for a right action, with the algebra (the
-- fibre) before the coalgebra (the base). The twisting cochain still goes
-- from the coalgebra to the algebra.
data TwistedTensor a c = TwistedTensor
  { twistedAlgebra :: a,
    twistedCoalgebra :: c,
    twistingCochain :: Morphism c a
  }

-- | The perturbation on @A ⊗ C@ determined by a twisting cochain
-- @C -> A@ and the right action of @A@ on itself.
perturbationForCochain ::
  (Algebra a, Coalgebra c) =>
  a ->
  c ->
  Morphism c a ->
  Morphism (Tensor a c) (Tensor a c)
perturbationForCochain a c tauMor = delta
  where
    (ClosedMorphism _ delta _) =
      (mu ⊗ idC)
        . assocInv
        . (idA ⊗ (tau ⊗ idC))
        . (idA ⊗ del)

    mu = ClosedMorphism (Tensor a a) (muMor a) a
    del = ClosedMorphism c (delMor c) (Tensor c c)
    tau = ClosedMorphism c tauMor a
    idA = ClosedMorphism a id a
    idC = ClosedMorphism c id c
    assocInv =
      ClosedMorphism
        (Tensor a (Tensor a c))
        tensorAssocInv
        (Tensor (Tensor a a) c)
    (⊗) = tensorFuncArr

-- | Recover the twisting cochain from a perturbation on an algebra-first
-- tensor product. The composite is @η ⊗ 1@, the perturbation, and
-- @1 ⊗ ε@.
cochainForPerturbation ::
  (Algebra a, Coalgebra c) =>
  a ->
  c ->
  Morphism (Tensor a c) (Tensor a c) ->
  Morphism c a
cochainForPerturbation a c delta =
  tensorUnitR
    . tensorFunc a c id (counitMor c)
    . delta
    . tensorFunc () c (unitMor a) id
    . tensorUnitLInv

twistedTensorPerturbation ::
  (Algebra a, Coalgebra c) =>
  TwistedTensor a c ->
  Morphism (Tensor a c) (Tensor a c)
twistedTensorPerturbation (TwistedTensor a c tau) =
  perturbationForCochain a c tau

instance (Algebra a, Coalgebra c) => ChainComplex (TwistedTensor a c) where
  type Basis (TwistedTensor a c) = (Basis a, Basis c)

  isBasis (TwistedTensor a c _) (s, t) = isBasis a s && isBasis c t
  degree (TwistedTensor a c _) = degree (Tensor a c)

  diff twisted@(TwistedTensor a c _) =
    sameBasisMorphism $
      diff (Perturbed (Tensor a c) (twistedTensorPerturbation twisted))

toTwisted ::
  (Algebra a, Coalgebra c) =>
  Perturbed (Tensor a c) ->
  TwistedTensor a c
toTwisted (Perturbed (Tensor a c) delta) =
  TwistedTensor a c (cochainForPerturbation a c delta)

fromTwisted ::
  (Algebra a, Coalgebra c) =>
  TwistedTensor a c ->
  Perturbed (Tensor a c)
fromTwisted twisted@(TwistedTensor a c _) =
  Perturbed (Tensor a c) (twistedTensorPerturbation twisted)

isoPerturbed ::
  (Algebra a, Coalgebra c) =>
  Morphism (Perturbed (Tensor a c)) (TwistedTensor a c)
isoPerturbed = basisMorphism id

isoPerturbedInv ::
  (Algebra a, Coalgebra c) =>
  Morphism (TwistedTensor a c) (Perturbed (Tensor a c))
isoPerturbedInv = basisMorphism id
