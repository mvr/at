-- | The free tensor algebra on a chain complex and the tensor trick for
-- lifting reductions through it.
module Math.Algebra.ChainComplex.TensorAlgebra where

import Control.Category.Constrained ((.))
import Prelude hiding ((.))

import Math.Algebra.ChainComplex
import Math.Algebra.ChainComplex.Algebra
import Math.Algebra.ChainComplex.Reduction
import Math.Algebra.ChainComplex.Tensor
import Math.Algebra.Combination

-- | The free tensor algebra on a chain complex. Basis elements are words in
-- basis elements of the generating complex.
newtype TensorAlgebra a = TensorAlgebra a

tensorAlgebraInclusion ::
  ChainComplex a =>
  Morphism a (TensorAlgebra a)
tensorAlgebraInclusion = basisMorphism pure

instance ChainComplex a => ChainComplex (TensorAlgebra a) where
  type Basis (TensorAlgebra a) = [Basis a]

  isBasis (TensorAlgebra a) = all (isBasis a)
  degree (TensorAlgebra a) = sum . fmap (degree a)
  diff (TensorAlgebra a) =
    tensorAlgebraDerivation a (tensorAlgebraInclusion . diff a)

instance ChainComplex a => Algebra (TensorAlgebra a) where
  unitMor _ = basisMorphism (const [])
  muMor _ = basisMorphism (uncurry (++))

instance ChainComplex a => AugmentedAlgebra (TensorAlgebra a) where
  augmentationMor _ = Morphism 0 $ \w ->
    if null w then singleComb () else zeroCombination

-- There is no general FiniteType instance: a degree-zero generator gives
-- arbitrarily long words in total degree zero.
-- A negative-degree generator also rules out a general BoundedBelow instance.

-- | Enumerate words of fixed length and total generator degree. A lower
-- bound makes the degree splits finite even when it is zero or negative;
-- fixing the word length is essential in those cases.
tensorWords ::
  (FiniteType a, BoundedBelow a) =>
  a ->
  Int -> -- Total generator degree
  Int -> -- Word length
  [[Basis a]]
tensorWords a d l = go (d - l * lo) l
  where
    lo = lowerBound a

    -- n is the excess degree above the minimum for the remaining letters.
    go 0 0 = [[]]
    go _ l | l <= 0 = []
    go n l = do
      i <- [0 .. n]
      b <- basis a (lo + i)
      bs <- go (n - i) (l - 1)
      pure (b : bs)

-- | Extend a degree-zero morphism multiplicatively to tensor words.
-- Nonzero degrees are rejected: on words of length n, the degree shift
-- would be n times the input degree, not constant across the tensor algebra.
tensorAlgebraFunc ::
  Morphism a b ->
  Morphism (TensorAlgebra a) (TensorAlgebra b)
tensorAlgebraFunc (Morphism d f)
  | d /= 0 = error "tensorAlgebraFunc: expected a degree-zero morphism"
  | otherwise = Morphism 0 (traverseCombination f)

tensorAlgebraDerivation ::
  ChainComplex a =>
  a ->
  Morphism a (TensorAlgebra a) ->
  Morphism (TensorAlgebra a) (TensorAlgebra a)
tensorAlgebraDerivation a f = Morphism d go
  where
    d = morphismDegree f

    go [] = zeroCombination
    go (b : bs) =
      mapCombination (++ bs) (f `onBasis` b)
        + kozulRule
          (d * degree a b)
          (mapMonotonic (b :) (go bs))

tensorAlgebraPairCoderivation ::
  ChainComplex a =>
  a ->
  Morphism (Tensor a a) a ->
  Morphism (TensorAlgebra a) (TensorAlgebra a)
tensorAlgebraPairCoderivation a f = Morphism d go
  where
    d = morphismDegree f

    go [] = zeroCombination
    go [_] = zeroCombination
    go (b : b' : bs) =
      mapMonotonic (: bs) (f `onBasis` (b, b'))
        + kozulRule
          (d * degree a b)
          (mapMonotonic (b :) (go (b' : bs)))

-- | The tensor trick extending a homotopy from the generators to their free
-- tensor algebras.
tensorAlgebraHomotopy ::
  ChainComplex a =>
  a ->
  Morphism a a ->
  Morphism a a ->
  Morphism (TensorAlgebra a) (TensorAlgebra a)
tensorAlgebraHomotopy a h gf = Morphism 1 go
  where
    tgf = tensorAlgebraFunc gf

    go [] = zeroCombination
    go (b : bs) =
      liftCombination2
        (:)
        (h `onBasis` b)
        (tgf `onBasis` bs)
        + kozulRule
          (degree a b)
          (mapMonotonic (b :) (go bs))

-- | Lift a reduction through the free tensor-algebra functor.
tensorAlgebraReduction ::
  (ChainComplex a, ChainComplex b) =>
  a ->
  Reduction a b ->
  Reduction (TensorAlgebra a) (TensorAlgebra b)
tensorAlgebraReduction a (Reduction f g h) =
  Reduction
    (tensorAlgebraFunc f)
    (tensorAlgebraFunc g)
    (tensorAlgebraHomotopy a h (g . f))
