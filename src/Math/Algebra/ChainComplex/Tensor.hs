{-# LANGUAGE UndecidableInstances #-}

-- | Tensor product of chain complexes of free Z-modules
module Math.Algebra.ChainComplex.Tensor where

import Control.Category.Constrained hiding (fmap)
import Math.Algebra.ChainComplex
import Math.Algebra.ChainComplex.Reduction
import Prelude hiding (id, (.), return)

data Tensor a b = Tensor a b

signFor :: Int -> Int
signFor x = if even x then 1 else -1

instance (ChainComplex a, ChainComplex b, Eq (Basis a), Eq (Basis b)) => ChainComplex (Tensor a b) where
  type Basis (Tensor a b) = (Basis a, Basis b)
  isBasis (Tensor a b) (s, t) = isBasis a s && isBasis b t
  degree (Tensor a b) (s, t) = degree a s + degree b t
  diff (Tensor a b) = Morphism (-1) go
    where
      go (s, t) =
        fmap (,t) (diff a `onBasis` s)
          + (signFor $ degree a s) .* fmap (s,) (diff b `onBasis` t)

instance (FiniteType a, FiniteType b, Eq (Basis a), Eq (Basis b)) => FiniteType (Tensor a b) where
  dim (Tensor a b) n = sum [dim a i * dim b (n - i) | i <- [0 .. n]]
  basis (Tensor a b) n =
    [(s, t) | i <- [0 .. n], s <- basis a i, t <- basis b (n - i)]

tensorCombination :: Combination a -> Combination b -> Combination (a, b)
tensorCombination = undefined

tensorMorphism ::
  (ChainComplex a1, ChainComplex a2) =>
  a1 ->
  a2 ->
  b1 ->
  b2 ->
  Morphism a1 b1 ->
  Morphism a2 b2 ->
  Morphism (Tensor a1 a2) (Tensor b1 b2)
tensorMorphism a1 a2 _ _ (Morphism deg f1) (Morphism _ f2) = Morphism deg ft
  where
    ft (x1, x2) = (signFor (degree a1 x1 * degree a2 x2)) .* tensorCombination (f1 x1) (f2 x2)

tensorAssoc :: a -> b -> c -> Morphism (Tensor (Tensor a b) c) (Tensor a (Tensor b c))
tensorAssoc a b c = Morphism 0 $ \((a,b), c) -> return (a, (b,c))

tensorAssocInv :: a -> b -> c -> Morphism (Tensor a (Tensor b c))  (Tensor (Tensor a b) c)
tensorAssocInv a b c = Morphism 0 $ \(a, (b, c)) -> return ((a, b), c)

tensorReduction ::
  (ChainComplex a1, ChainComplex a2, ChainComplex b1, ChainComplex b2, Eq (Basis a2), Eq (Basis b2), Eq (Basis a1)) =>
  a1 ->
  a2 ->
  b1 ->
  b2 ->
  Reduction a1 b1 ->
  Reduction a2 b2 ->
  Reduction (Tensor a1 a2) (Tensor b1 b2)
tensorReduction a1 a2 b1 b2 (Reduction f1 g1 h1) (Reduction f2 g2 h2) = Reduction f' g' h'
  where
    f' = tensorMorphism a1 a2 b1 b2 f1 f2
    g' = tensorMorphism b1 b2 a1 a2 g1 g2
    h' = (tensorMorphism a1 a2 a1 a2 h1 (g2 . f2)) + (tensorMorphism a1 a2 a1 a2 id h2)

-- Convenience:
tensorMorphismArr ::
  (ChainComplex a1, ChainComplex a2) =>
  ClosedMorphism a1 b1 ->
  ClosedMorphism a2 b2 ->
  ClosedMorphism (Tensor a1 a2) (Tensor b1 b2)
tensorMorphismArr (ClosedMorphism a1 m1 b1) (ClosedMorphism a2 m2 b2) = ClosedMorphism (Tensor a1 a2) (tensorMorphism a1 a2 b1 b2 m1 m2) (Tensor b1 b2)

-- tensorReductionArr ::
--   (ChainComplex a1, ChainComplex a2) =>
--   ClosedReduction a1 b1 ->
--   ClosedReduction a2 b2 ->
--   ClosedReduction (Tensor a1 a2) (Tensor b1 b2)
-- tensorReductionArr (ClosedReduction a1 m1 b1) (ClosedReduction a2 m2 b2) = ClosedReduction (Tensor a1 a2) (tensorReduction a1 a2 b1 b2 m1 m2) (Tensor b1 b2)
