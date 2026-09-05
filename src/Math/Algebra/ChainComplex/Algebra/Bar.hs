-- | The reduced bar construction \(Bar(ℤ,A,ℤ)\) of a connected augmented
-- DG-algebra \(A\).
--
-- There are many resources that describe this bar construction. For
-- example, see Section 2.2.2 in
-- <http://math.uchicago.edu/~may/REU2019/REUPapers/Zhang,Ruoqi(Rachel).pdf>.
-- Also <https://ncatlab.org/nlab/show/bar+and+cobar+construction>,
-- and Homology, MacLane, Chapter X.10 (Kenzo claims there is a sign
-- error)
--
-- For the commutative algebra structure see for example
-- <https://doi.org/10.1023/A:1013544506151>
--
-- The underlying complex is the tensor algebra on the suspended
-- augmentation ideal. The Bar differential adds the multiplication
-- perturbation to its internal differential. The bicomplex presentation
-- records word length and internal degree; its total complex agrees with
-- this perturbed tensor algebra.
module Math.Algebra.ChainComplex.Algebra.Bar where

-- There are lots of places that the signs can go wrong.
--
-- Whatever we do should end up compatible with the sign choices made
-- by Kenzo, so we can confirm things are going right.
--
-- TODO: compare sign choices with
-- https://www-users.cse.umn.edu/~tlawson/papers/signs.pdf
-- Not promising: "This brings us to a dear friend whose sign
-- conventions have personally given me nightmares on more than one
-- occasion. Namely, the bar construction — or specifically, in this
-- case, the bar construction of a differential graded algebra with
-- coefficients in a pair of differential graded modules. "

import Control.Category.Constrained ((.))
import Prelude hiding ((.))

import Math.Algebra.Bicomplex hiding (FiniteType)
import qualified Math.Algebra.Bicomplex as Bi (FiniteType)
import Math.Algebra.ChainComplex
import Math.Algebra.ChainComplex.Algebra
import Math.Algebra.ChainComplex.Equivalence
import Math.Algebra.ChainComplex.Reduction
import Math.Algebra.ChainComplex.Shift
import Math.Algebra.ChainComplex.TensorAlgebra
import Math.Algebra.ChainComplex.Truncation
import Math.Algebra.Combination

-- | The tensor algebra on the suspended positive-degree truncation.
-- For a connected augmented complex, this truncation represents its
-- augmentation ideal.
newtype BarTensor a = BarTensor a

barTensorUnderlying :: BarTensor a -> TensorAlgebra (Susp (NaiveTruncation a))
barTensorUnderlying (BarTensor a) =
  TensorAlgebra (Susp (NaiveTruncation 1 a))

instance ChainComplex a => Bicomplex (BarTensor a) where
  type Bibasis (BarTensor a) = [Basis a]

  isBibasis t = isBasis (barTensorUnderlying t)

  bidegree (BarTensor a) bs = (length bs, sum (degree a <$> bs))

  vdiff t =
    verticaliseMorphism (diff (barTensorUnderlying t))

  hdiff _ = bimorphismZeroOfDeg (Bidegree (-1, 0))

instance ChainComplex a => ChainComplex (BarTensor a) where
  type Basis (BarTensor a) = [Basis a]
  isBasis t = isBasis (barTensorUnderlying t)
  degree t = degree (barTensorUnderlying t)
  diff t = sameBasisMorphism (diff (barTensorUnderlying t))

instance ChainComplex a => BoundedBelow (BarTensor a) where
  lowerBound _ = 0

instance FiniteType a => Bi.FiniteType (BarTensor a) where
  bibasis (BarTensor a) (h, v) =
    tensorWords (NaiveTruncation 1 a) v h

instance FiniteType a => FiniteType (BarTensor a) where
  basis t = basis (Tot t)

instance ChainComplex a => Algebra (BarTensor a) where
  unitMor t = sameBasisMorphism (unitMor (barTensorUnderlying t))
  muMor t = sameBasisMorphism (muMor (barTensorUnderlying t))

instance ChainComplex a => AugmentedAlgebra (BarTensor a) where
  augmentationMor t =
    sameBasisMorphism (augmentationMor (barTensorUnderlying t))

-- Every suspended generator has degree at least two, independently of
-- the grading of the original complex.
instance ChainComplex a => ConnectedChainComplex (BarTensor a)

instance ChainComplex a => OneReducedChainComplex (BarTensor a)

-- | Lift a reduction to the unperturbed tensor algebras underlying Bar.
--
-- WARNING: The reduction must respect the degree-zero splitting: truncating
-- source and target below degree one must preserve the reduction identities.
-- This is unchecked. Naive truncation does not preserve arbitrary homotopies;
-- for example, truncating the contraction of @Disk 1@ creates a degree-one cycle.
barTensorReduction ::
  forall a b.
  (ChainComplex a, ChainComplex b) =>
  a -> Reduction a b -> Reduction (BarTensor a) (BarTensor b)
barTensorReduction a r =
  sameBasisReduction $
    tensorAlgebraReduction
      (Susp (NaiveTruncation 1 a))
      (suspReduction (sameBasisReduction r :: Reduction (NaiveTruncation a) (NaiveTruncation b)))

newtype Bar a = Bar a

-- | The multiplication part of the Bar differential. It merges adjacent
-- suspended generators with the Koszul sign determined by their prefix.
barPerturbation ::
  (AugmentedAlgebra a, ConnectedChainComplex a) =>
  a ->
  Morphism (BarTensor a) (BarTensor a)
barPerturbation a =
  sameBasisMorphism $
    tensorAlgebraPairCoderivation
      (Susp i)
      (sameBasisMorphism (muMor a) . tensorSusp i i)
  where
    i = NaiveTruncation 1 a

instance (AugmentedAlgebra a, ConnectedChainComplex a) => Bicomplex (Bar a) where
  type Bibasis (Bar a) = [Basis a]

  isBibasis (Bar a) = isBibasis (BarTensor a)
  bidegree (Bar a) = bidegree (BarTensor a)
  vdiff (Bar a) = sameBibasisMorphism (vdiff (BarTensor a))

  hdiff (Bar a) =
    horizontaliseMorphism (barPerturbation a)

instance (AugmentedAlgebra a, ConnectedChainComplex a, FiniteType a) => Bi.FiniteType (Bar a) where
  bibasis (Bar a) = bibasis (BarTensor a)

instance (AugmentedAlgebra a, ConnectedChainComplex a) => ChainComplex (Bar a) where
  type Basis (Bar a) = [Basis a]
  isBasis (Bar a) = isBasis (BarTensor a)
  degree (Bar a) = degree (BarTensor a)
  diff (Bar a) =
    sameBasisMorphism $
      diff (Perturbed (BarTensor a) (barPerturbation a))

instance (AugmentedAlgebra a, ConnectedChainComplex a) => BoundedBelow (Bar a) where
  lowerBound (Bar a) = lowerBound (BarTensor a)

instance (AugmentedAlgebra a, ConnectedChainComplex a, FiniteType a) => FiniteType (Bar a) where
  basis (Bar a) = basis (BarTensor a)

instance (AugmentedAlgebra a, ConnectedChainComplex a) => ConnectedChainComplex (Bar a)

instance (AugmentedAlgebra a, ConnectedChainComplex a) => OneReducedChainComplex (Bar a)

shuffle :: (ChainComplex a) => a -> [Basis a] -> [Basis a] -> Combination [Basis a]
shuffle c as [] = singleComb as
shuffle c [] bs = singleComb bs
shuffle c (a : as) (b : bs) =
  mapMonotonic (a :) (shuffle c as (b : bs))
    + kozulRule eps (mapMonotonic (b :) (shuffle c (a : as) bs))
  where
    eps = degree (Susp c) b * degree (BarTensor c) (a : as)

instance (CommAlgebra a, AugmentedAlgebra a, ConnectedChainComplex a) => Algebra (Bar a) where
  unitMor (Bar a) = sameBasisMorphism (unitMor (BarTensor a))
  muMor (Bar a) = Morphism 0 (uncurry (shuffle a))

instance (CommAlgebra a, AugmentedAlgebra a, ConnectedChainComplex a) => AugmentedAlgebra (Bar a) where
  augmentationMor (Bar a) =
    sameBasisMorphism (augmentationMor (BarTensor a))

instance (CommAlgebra a, AugmentedAlgebra a, ConnectedChainComplex a) => CommAlgebra (Bar a)

-- | Apply Bar to a degree-zero, augmentation-preserving algebra chain
-- map. Preservation of the differential, multiplication, unit, and
-- augmentation is assumed, not checked.
barFunc ::
  ( AugmentedAlgebra a,
    AugmentedAlgebra b,
    ConnectedChainComplex a,
    ConnectedChainComplex b
  ) =>
  Morphism a b ->
  Morphism (Bar a) (Bar b)
barFunc f = sameBasisMorphism (tensorAlgebraFunc f)

-- | Transfer the multiplication perturbation through a strong
-- equivalence. Both reductions of the supplied equivalence must
-- respect the degree-zero splitting required by 'barTensorReduction',
-- including at the common apex. This condition is unchecked. No algebra
-- structure is required on the target; the transferred perturbation
-- retains the higher corrections.
barEquiv ::
  (AugmentedAlgebra a, ConnectedChainComplex a, ChainComplex b) =>
  Equivalence a b ->
  Equivalence (Bar a) (Perturbed (BarTensor b))
barEquiv (Equivalence a l x r b) =
  sameBasisEquiv (Bar a) (equivRight e) e
  where
    e =
      perturbLeft
        ( Equivalence
            (BarTensor a)
            (barTensorReduction x l)
            (BarTensor x)
            (barTensorReduction x r)
            (BarTensor b)
        )
        (barPerturbation a)

-- TODO: universal twisting cochain a -> Bar a (should be same as the one induced by the twist on Wbar)
