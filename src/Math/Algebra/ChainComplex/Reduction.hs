-- | A strong deformation retract of chain complexes.  We follow Kenzo
-- and call these 'reductions'. In other places these are called
-- 'contractions' or 'SDR-data'.
module Math.Algebra.ChainComplex.Reduction where

import Control.Category.Constrained
import Math.Algebra.ChainComplex

import Prelude hiding (fmap, id, (.))

data Reduction a b = Reduction
  { reductionF :: Morphism a b, -- degree 0
    reductionG :: Morphism b a, -- degree 0
    reductionH :: Morphism a a -- degree 1
  }

-- | Retag a reduction with unchanged endpoint basis types.
sameBasisReduction ::
  (Basis a ~ Basis a', Basis b ~ Basis b') =>
  Reduction a b ->
  Reduction a' b'
sameBasisReduction (Reduction project include homotopy) =
  Reduction
    (sameBasisMorphism project)
    (sameBasisMorphism include)
    (sameBasisMorphism homotopy)

instance Semigroupoid Reduction where
  type Object Reduction a = ChainComplex a
  (Reduction f1 g1 h1) . (Reduction f2 g2 h2) = Reduction (f1 . f2) (g2 . g1) (h2 + (g2 . h1 . f2))

instance Category Reduction where
  id = Reduction id id (morphismZeroOfDeg 1)

isoToReduction :: Iso Morphism a b -> Reduction a b
isoToReduction (Iso f g) = Reduction f g (morphismZeroOfDeg 1)

data Perturbed a = Perturbed
  { perturbedOrig :: a,
    perturbedDiff :: Morphism a a
  }

instance (ChainComplex a) => ChainComplex (Perturbed a) where
  type Basis (Perturbed a) = Basis a
  isBasis (Perturbed a _) = isBasis a
  degree (Perturbed a _) = degree a
  diff (Perturbed a delta) = sameBasisMorphism (diff a + delta)

instance BoundedBelow a => BoundedBelow (Perturbed a) where
  lowerBound (Perturbed a _) = lowerBound a

instance ConnectedChainComplex a => ConnectedChainComplex (Perturbed a)

instance OneReducedChainComplex a => OneReducedChainComplex (Perturbed a)

instance (FiniteType a) => FiniteType (Perturbed a) where
  dim (Perturbed a _) = dim a
  basis (Perturbed a _) = basis a

liftPerturbedMorphism :: Morphism a b -> Morphism (Perturbed a) (Perturbed b)
liftPerturbedMorphism = sameBasisMorphism

-- | The Basic Perturbation Lemma
-- The recursion only terminates if (deltahat . h) is
-- pointwise nilpotent, and this is not checked!.
perturb ::
  (ChainComplex a, ChainComplex b) =>
  a ->
  b ->
  Reduction a b ->
  Morphism a a ->
  (Perturbed a, Perturbed b, Reduction (Perturbed a) (Perturbed b))
perturb a b (Reduction f g h) deltahat =
  ( Perturbed a deltahat,
    Perturbed b delta,
    sameBasisReduction (Reduction f' g' h')
  )
  where
    -- Write psi = (1 + deltahat h)^-1.  The right-hand formulas let the
    -- transferred differential project during the recursion, instead of first
    -- materialising sigma g for sigma = (1 + h deltahat)^-1.
    deltaH = deltahat . h
    f'imp d = f `onBasis` d - f' `onComb` (deltaH `onBasis` d)
    h'imp d = h `onBasis` d - h' `onComb` (deltaH `onBasis` d)
    f' = memoiseMorphism $ Morphism 0 f'imp
    h' = Morphism 1 h'imp
    g' = memoiseMorphism $ g - h' . deltahat . g
    delta = f' . deltahat . g

-- | Use the BPL to set the differential of `a` to a particular
-- morphism. Again, the nilpotence condition of the BPL must be
-- satisfied.
perturbTo ::
  (ChainComplex a, ChainComplex b) =>
  a ->
  b ->
  Reduction a b ->
  Morphism a a ->
  (Perturbed a, Perturbed b, Reduction (Perturbed a) (Perturbed b))
perturbTo a b r d = perturb a b r (d - diff a)

-- | The Easy Perturbation Lemma
perturbBottom ::
  (ChainComplex a, ChainComplex b) =>
  a ->
  b ->
  Reduction a b ->
  Morphism b b ->
  (Perturbed a, Perturbed b, Reduction (Perturbed a) (Perturbed b))
perturbBottom a b (Reduction f g h) delta =
  ( Perturbed a deltahat,
    Perturbed b delta,
    sameBasisReduction (Reduction f g h)
  )
  where
    deltahat = memoiseMorphism $ g . delta . f

-- | Use the EPL to set the differential of `b` to a particular
-- morphism.
perturbBottomTo ::
  (ChainComplex a, ChainComplex b) =>
  a ->
  b ->
  Reduction a b ->
  Morphism b b ->
  (Perturbed a, Perturbed b, Reduction (Perturbed a) (Perturbed b))
perturbBottomTo a b r d = perturbBottom a b r (d - diff b)
