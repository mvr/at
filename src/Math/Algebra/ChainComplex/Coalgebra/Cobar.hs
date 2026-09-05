-- | The Cobar construction of a one-reduced coaugmented DG-coalgebra.
--
-- The underlying graded module is the tensor algebra
-- \(T(s^{-1}\bar C)\). A word stores basis elements of \(C\), while its
-- degree accounts for one desuspension per letter. The degree-zero
-- coaugmentation and the absent degree-one part are omitted.
module Math.Algebra.ChainComplex.Coalgebra.Cobar where

import Control.Category.Constrained ((.))
import Prelude hiding ((.))

import Math.Algebra.Bicomplex hiding (FiniteType)
import qualified Math.Algebra.Bicomplex as Bi (FiniteType (..))
import Math.Algebra.ChainComplex
import Math.Algebra.ChainComplex.Algebra
import Math.Algebra.ChainComplex.Coalgebra
import Math.Algebra.ChainComplex.Equivalence
import Math.Algebra.ChainComplex.Reduction
import Math.Algebra.ChainComplex.Shift
import Math.Algebra.ChainComplex.Tensor
import Math.Algebra.ChainComplex.TensorAlgebra
import Math.Algebra.ChainComplex.Truncation

-- | The tensor algebra on the desuspended degree-at-least-two truncation.
-- For a one-reduced coaugmented complex, this truncation represents its
-- coaugmentation coideal.
newtype CobarTensor c = CobarTensor c

cobarTensorUnderlying :: CobarTensor c -> TensorAlgebra (Desusp (NaiveTruncation c))
cobarTensorUnderlying (CobarTensor c) =
  TensorAlgebra (Desusp (NaiveTruncation 2 c))

instance ChainComplex c => Bicomplex (CobarTensor c) where
  type Bibasis (CobarTensor c) = [Basis c]

  isBibasis t = isBasis (cobarTensorUnderlying t)
  bidegree (CobarTensor c) bs = (-length bs, sum (degree c <$> bs))
  hdiff _ = bimorphismZeroOfDeg (Bidegree (-1, 0))
  vdiff t = verticaliseMorphism (diff (cobarTensorUnderlying t))

instance ChainComplex c => ChainComplex (CobarTensor c) where
  type Basis (CobarTensor c) = [Basis c]

  isBasis t = isBasis (cobarTensorUnderlying t)
  degree t = degree (cobarTensorUnderlying t)
  diff t = sameBasisMorphism (diff (cobarTensorUnderlying t))

instance ChainComplex c => BoundedBelow (CobarTensor c) where
  lowerBound _ = 0

instance FiniteType c => Bi.FiniteType (CobarTensor c) where
  bibasis (CobarTensor c) (h, v)
    | h > 0 = []
    | otherwise = tensorWords (NaiveTruncation 2 c) v (-h)

  -- Second quadrant
  totalBidegrees _ d
    | d < 0 = []
    | otherwise = (\n -> (-n, d + n)) <$> [0 .. d]

instance FiniteType c => FiniteType (CobarTensor c) where
  basis t = basis (Tot t)

instance ChainComplex c => ConnectedChainComplex (CobarTensor c)

instance ChainComplex c => Algebra (CobarTensor c) where
  unitMor t = sameBasisMorphism (unitMor (cobarTensorUnderlying t))
  muMor t = sameBasisMorphism (muMor (cobarTensorUnderlying t))

instance ChainComplex c => AugmentedAlgebra (CobarTensor c) where
  augmentationMor t = sameBasisMorphism (augmentationMor (cobarTensorUnderlying t))

-- | Lift a reduction to the unperturbed tensor algebras underlying Cobar.
--
-- WARNING: The reduction must respect the low-degree splitting: truncating
-- source and target below degree two must preserve the reduction identities.
-- This is unchecked. Naive truncation does not preserve arbitrary homotopies;
-- for example, truncating the contraction of @Disk 2@ creates a degree-two cycle.
cobarTensorReduction ::
  forall c d.
  (ChainComplex c, ChainComplex d) =>
  c -> Reduction c d -> Reduction (CobarTensor c) (CobarTensor d)
cobarTensorReduction c r =
  sameBasisReduction $
    tensorAlgebraReduction
      (Desusp (NaiveTruncation 2 c))
      (desuspReduction (sameBasisReduction r :: Reduction (NaiveTruncation c) (NaiveTruncation d)))

-- | Lift an equivalence to the unperturbed tensor algebras underlying Cobar.
-- Both reductions must respect the low-degree splitting required by
-- 'cobarTensorReduction', including at the common apex.
cobarTensorEquiv :: Equivalence c d -> Equivalence (CobarTensor c) (CobarTensor d)
cobarTensorEquiv (Equivalence c l x r d) =
  Equivalence
    (CobarTensor c)
    (cobarTensorReduction x l)
    (CobarTensor x)
    (cobarTensorReduction x r)
    (CobarTensor d)

-- | The Cobar construction, including the differential induced by the
-- reduced coproduct.
newtype Cobar c = Cobar c

-- | The reduced coproduct on a desuspended generator. The sign is
-- @d_delta(s^-1 c) = sum (-1)^|c'| (s^-1 c' | s^-1 c'')@.
cobarGeneratorPerturbation ::
  (CoaugmentedCoalgebra c, OneReducedChainComplex c) =>
  c -> Morphism (Desusp (NaiveTruncation c)) (TensorAlgebra (Desusp (NaiveTruncation c)))
cobarGeneratorPerturbation c =
  muMor t
    . tensorFunc d d tensorAlgebraInclusion tensorAlgebraInclusion
    . desuspTensor q q
    . sameBasisMorphism (reducedDelMor c)
  where
    q = NaiveTruncation 2 c
    d = Desusp q
    t = TensorAlgebra d

-- | The horizontal part of the Cobar differential, expressed as a
-- perturbation of the tensor algebra on the desuspended coaugmentation
-- coideal.
cobarPerturbation ::
  (CoaugmentedCoalgebra c, OneReducedChainComplex c) =>
  c -> Morphism (CobarTensor c) (CobarTensor c)
cobarPerturbation c =
  sameBasisMorphism $
    tensorAlgebraDerivation
      (Desusp (NaiveTruncation 2 c))
      (cobarGeneratorPerturbation c)

instance (CoaugmentedCoalgebra c, OneReducedChainComplex c) => Bicomplex (Cobar c) where
  type Bibasis (Cobar c) = [Basis c]

  isBibasis (Cobar c) = isBibasis (CobarTensor c)
  bidegree (Cobar c) = bidegree (CobarTensor c)
  vdiff (Cobar c) = sameBibasisMorphism (vdiff (CobarTensor c))
  hdiff (Cobar c) = horizontaliseMorphism (cobarPerturbation c)

instance (CoaugmentedCoalgebra c, OneReducedChainComplex c, FiniteType c) => Bi.FiniteType (Cobar c) where
  bibasis (Cobar c) = bibasis (CobarTensor c)
  totalBidegrees (Cobar c) = totalBidegrees (CobarTensor c)

instance (CoaugmentedCoalgebra c, OneReducedChainComplex c) => ChainComplex (Cobar c) where
  type Basis (Cobar c) = [Basis c]

  isBasis (Cobar c) = isBasis (CobarTensor c)
  degree (Cobar c) = degree (CobarTensor c)
  diff (Cobar c) =
    sameBasisMorphism $
      diff (Perturbed (CobarTensor c) (cobarPerturbation c))

instance (CoaugmentedCoalgebra c, OneReducedChainComplex c) => BoundedBelow (Cobar c) where
  lowerBound (Cobar c) = lowerBound (CobarTensor c)

instance (CoaugmentedCoalgebra c, OneReducedChainComplex c) => ConnectedChainComplex (Cobar c)

instance (CoaugmentedCoalgebra c, OneReducedChainComplex c, FiniteType c) => FiniteType (Cobar c) where
  basis (Cobar c) = basis (CobarTensor c)

instance (CoaugmentedCoalgebra c, OneReducedChainComplex c) => Algebra (Cobar c) where
  unitMor (Cobar c) = sameBasisMorphism (unitMor (CobarTensor c))
  muMor (Cobar c) = sameBasisMorphism (muMor (CobarTensor c))

instance (CoaugmentedCoalgebra c, OneReducedChainComplex c) => AugmentedAlgebra (Cobar c) where
  augmentationMor (Cobar c) = sameBasisMorphism (augmentationMor (CobarTensor c))

-- | Lift a degree-zero coaugmented DG-coalgebra map to Cobar.
-- The input must commute with the differential and preserve the coproduct,
-- counit, and coaugmentation. Only the degree-zero requirement is checked.
cobarFunc ::
  ( CoaugmentedCoalgebra c,
    CoaugmentedCoalgebra d,
    OneReducedChainComplex c,
    OneReducedChainComplex d
  ) =>
  Morphism c d -> Morphism (Cobar c) (Cobar d)
cobarFunc f = sameBasisMorphism (tensorAlgebraFunc f)

-- | Transfer the Cobar coproduct perturbation across a strong equivalence.
--
-- WARNING: Both reductions must satisfy the low-degree-splitting precondition
-- of 'cobarTensorReduction', including at the common apex.
-- One-reduced endpoints alone do not suffice. This condition is unchecked
-- and is not preserved by general 'Equivalence' composition: even composing
-- identity equivalences can introduce contractible degree-one/degree-two
-- pairs in the bicone apex whose contraction is destroyed by truncation.
--
-- Pointwise termination follows from the word-length filtration: the
-- perturbation raises word length, while a word in total degree n has length
-- at most n.
cobarEquiv ::
  (CoaugmentedCoalgebra c, OneReducedChainComplex c, ChainComplex d) =>
  Equivalence c d -> Equivalence (Cobar c) (Perturbed (CobarTensor d))
cobarEquiv e = sameBasisEquiv (Cobar c) (equivRight e') e'
  where
    c = equivLeft e
    e' = perturbLeft (cobarTensorEquiv e) (cobarPerturbation c)
