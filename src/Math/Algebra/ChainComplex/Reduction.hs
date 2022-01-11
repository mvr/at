{-# LANGUAGE UndecidableInstances #-}

-- | A strong deformation retract of chain complexes.
-- We follow Kenzo by shortening this to 'reduction'.
module Math.Algebra.ChainComplex.Reduction where

import Control.Category.Constrained
import Data.Coerce
import Math.Algebra.ChainComplex

import Prelude hiding (id, (.))

data UReduction a b = Reduction
  { reductionF :: UMorphism a b, -- degree 0
    reductionG :: UMorphism b a, -- degree 0
    reductionH :: UMorphism a a -- degree 1
  }

type Reduction a b = UReduction (Basis a) (Basis b)

instance Category UReduction where
  type Object UReduction a = Eq a
  id = Reduction id id (morphismZeroOfDeg 1)
  (Reduction f1 g1 h1) . (Reduction f2 g2 h2) = Reduction (f1 . f2) (g2 . g1) (h2 + (g2 . h1 . f2))

isoToReduction :: (Eq a) => UMorphism a b -> UMorphism b a -> UReduction a b
isoToReduction f g = Reduction f g 0

data Perturbed a = Perturbed a (Morphism a a)

newtype PerturbedBasis a = PerturbedBasis a
  deriving (Eq)

instance (ChainComplex a, Eq (Basis a)) => ChainComplex (Perturbed a) where
  type Basis (Perturbed a) = PerturbedBasis (Basis a)
  degree (Perturbed a _) (PerturbedBasis b) = degree a b
  diff (Perturbed a delta) = Morphism (-1) $ coerce $ \b -> diff a `onBasis` b + delta `onBasis` b

-- | The Basic Perturbation Lemma
-- The recursion only terminates if (deltahat . h) is
-- pointwise nilpotent.
perturb ::
  (Eq (Basis a), Eq (Basis b)) =>
  a ->
  b ->
  Reduction a b ->
  Morphism a a ->
  (Perturbed a, Perturbed b, Reduction (Perturbed a) (Perturbed b))
perturb a b (Reduction f g h) deltahat =
  (Perturbed a deltahat, Perturbed b delta, Reduction (coerce f') (coerce g') (coerce h'))
  where
    sigma = id - (h . deltahat . sigma)
    f' = f . (id - (deltahat . sigma . h))
    g' = sigma . g
    h' = sigma . h
    delta = f . deltahat . g'

-- | The Easy Perturbation Lemma
perturbBottom ::
  (Eq (Basis a), Eq (Basis b)) =>
  a ->
  b ->
  Reduction a b ->
  Morphism b b ->
  (Perturbed a, Perturbed b, Reduction (Perturbed a) (Perturbed b))
perturbBottom a b (Reduction f g h) delta =
  (Perturbed a deltahat, Perturbed b delta, Reduction (coerce f) (coerce g) (coerce h))
  where
    deltahat = g . delta . f
