-- | The bar construction of a DG-algebra \(A\), specifically,
-- \(Bar(ℤ,A,ℤ)\).  The bar construction of an ordinary algebra is a
-- special case (sometimes called the 'standard complex').
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
-- To reduce the surface area of where sign issues can creep in, the
-- construction is factored into two steps:
-- AugAlg(Z) -Bar-> biCh(Z) -Tot-> Ch(Z)
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
import Math.Algebra.Combination

-- To implement the action of `Bar` on reductions, we need the tensor
-- algebra functor, which only uses the vertical differentials of the Bar
-- bicomplex.
-- TODO: this could be moved to its own file

-- | The tensor algebra on the suspension of a chain complex. Every basis
-- element of the supplied complex is available as a tensor generator.
newtype TensorSusp a = TensorSusp a

-- | The positive-degree truncation of a chain complex. For a connected
-- augmented complex, this represents its augmentation ideal. The same
-- representation is also used for intermediate complexes in a strong
-- equivalence; those reductions are expected to respect the degree-zero
-- splitting.
newtype AugmentationIdeal a = AugmentationIdeal a

-- | The tensor algebra on the suspended augmentation ideal.
type BarTensor a = TensorSusp (AugmentationIdeal a)

-- | The unperturbed tensor algebra underlying the Bar construction.
barTensor :: a -> BarTensor a
barTensor = TensorSusp . AugmentationIdeal

instance ChainComplex a => Bicomplex (TensorSusp a) where
  type Bibasis (TensorSusp a) = [Basis a]

  isBibasis (TensorSusp a) = all (isBasis a)

  bidegree (TensorSusp a) bs = (length bs, sum (degree a <$> bs))

  vdiff (TensorSusp a) = Bimorphism (Bidegree (0, -1)) go
    where
      -- Homological suspension convention: d(sb) = -s(db).
      go :: [Basis a] -> Combination [Basis a]
      go [] = 0
      go (b : bs) =
        -mapMonotonic (: bs) (diff a `onBasis` b)
          + kozulRule (degree a b + 1) (mapMonotonic (b :) (go bs))

  hdiff _ = bimorphismZeroOfDeg (Bidegree (-1, 0))

instance ChainComplex a => ChainComplex (TensorSusp a) where
  type Basis (TensorSusp a) = [Basis a]
  isBasis (TensorSusp a) = isBasis (Tot (TensorSusp a))
  degree (TensorSusp a) = degree (Tot (TensorSusp a))
  diff (TensorSusp a) = sameBasisMorphism (diff (Tot (TensorSusp a)))

instance ChainComplex a => ChainComplex (AugmentationIdeal a) where
  type Basis (AugmentationIdeal a) = Basis a

  isBasis (AugmentationIdeal a) b =
    degree a b > 0 && isBasis a b
  degree (AugmentationIdeal a) = degree a
  diff (AugmentationIdeal a) = Morphism (-1) $ \b ->
    bindCombination (diff a `onBasis` b) $ \image ->
      if degree a image <= 0
        then zeroCombination
        else singleComb image

instance ChainComplex a => BoundedBelow (AugmentationIdeal a) where
  lowerBound _ = 1

instance FiniteType a => FiniteType (AugmentationIdeal a) where
  basis (AugmentationIdeal a) d
    | d <= 0 = []
    | otherwise = filter (isBasis (AugmentationIdeal a)) (basis a d)

tensorAlgFunc ::
  (ChainComplex a, ChainComplex b) =>
  Morphism a b ->
  Morphism (TensorSusp a) (TensorSusp b)
tensorAlgFunc (Morphism deg f) = Morphism deg (traverseCombination f)

augmentationIdealWords ::
  FiniteType a =>
  AugmentationIdeal a ->
  Int ->
  Int ->
  [[Basis a]]
augmentationIdealWords ideal d l = go (d - l * lo) l
  where
    lo = lowerBound ideal

    -- n is the excess degree above the minimum for the remaining letters.
    go 0 0 = [[]]
    go _ l | l <= 0 = []
    go n l = do
      i <- [0 .. n]
      b <- basis ideal (lo + i)
      bs <- go (n - i) (l - 1)
      pure (b : bs)

instance
  FiniteType a =>
  Bi.FiniteType (TensorSusp (AugmentationIdeal a))
  where
  bibasis (TensorSusp ideal) (horizontalDegree, verticalDegree)
    | horizontalDegree < 0 = []
    | otherwise =
        augmentationIdealWords ideal verticalDegree horizontalDegree

instance
  FiniteType a =>
  FiniteType (TensorSusp (AugmentationIdeal a))
  where
  basis tensorSusp = basis (Tot tensorSusp)

instance ChainComplex a => BoundedBelow (TensorSusp (AugmentationIdeal a)) where
  lowerBound _ = 0

instance ChainComplex a => Algebra (TensorSusp a) where
  unitMor _ = basisMorphism (const [])
  muMor _ = basisMorphism (\(left, right) -> left ++ right)

instance ChainComplex a => AugmentedAlgebra (TensorSusp a) where
  augmentationMor _ = Morphism 0 $ \word ->
    if null word then singleComb () else zeroCombination

instance
  ConnectedChainComplex a =>
  ConnectedChainComplex (TensorSusp (AugmentationIdeal a))

instance
  ConnectedChainComplex a =>
  OneReducedChainComplex (TensorSusp (AugmentationIdeal a))

verth :: ChainComplex a => a -> Morphism a a -> Morphism a a -> [Basis a] -> Combination [Basis a]
verth _ _ _ [] = 0
verth a h gf (b : bs) =
  -liftCombination2
    (:)
    (h `onBasis` b)
    (tensorAlgFunc gf `onBasis` bs)
    + kozulRule (degree a b + 1) (mapMonotonic (b :) (verth a h gf bs))

tensorAlgReduction ::
  (ChainComplex a, ChainComplex b) =>
  a ->
  b ->
  Reduction a b ->
  Reduction (TensorSusp a) (TensorSusp b)
tensorAlgReduction a b (Reduction f g h) =
  Reduction
    (tensorAlgFunc f)
    (tensorAlgFunc g)
    (Morphism 1 $ verth a h (g . f))

newtype Bar a = Bar a

instance (AugmentedAlgebra a, ConnectedChainComplex a) => Bicomplex (Bar a) where
  type Bibasis (Bar a) = [Basis a]

  isBibasis (Bar a) = isBibasis (barTensor a)
  bidegree (Bar a) = bidegree (barTensor a)
  vdiff (Bar a) =
    Bimorphism (Bidegree (0, -1)) (onBibasis (vdiff (barTensor a)))

  hdiff (Bar a) = Bimorphism (Bidegree (-1, 0)) go
    where
      go :: [Basis a] -> Combination [Basis a]
      go [] = 0
      go [b1] = 0
      go (b1 : b2 : bs) = kozulRule (degree a b1 + 1) (mapMonotonic (: bs) (muMor a `onBasis` (b1, b2)) + mapMonotonic (b1 :) (go (b2 : bs)))

instance
  (AugmentedAlgebra a, ConnectedChainComplex a, FiniteType a) =>
  Bi.FiniteType (Bar a)
  where
  bibasis (Bar a) = bibasis (barTensor a)

instance (AugmentedAlgebra a, ConnectedChainComplex a) => ChainComplex (Bar a) where
  type Basis (Bar a) = [Basis a]
  isBasis (Bar a) = isBasis (Tot (Bar a))
  degree (Bar a) = degree (Tot (Bar a))
  diff (Bar a) = sameBasisMorphism (diff (Tot (Bar a)))

instance (AugmentedAlgebra a, ConnectedChainComplex a) => BoundedBelow (Bar a) where
  lowerBound (Bar a) = lowerBound (barTensor a)

instance
  (AugmentedAlgebra a, ConnectedChainComplex a, FiniteType a) =>
  FiniteType (Bar a)
  where
  basis (Bar a) = basis (barTensor a)

instance
  (AugmentedAlgebra a, ConnectedChainComplex a) =>
  ConnectedChainComplex (Bar a)

instance
  (AugmentedAlgebra a, ConnectedChainComplex a) =>
  OneReducedChainComplex (Bar a)

shuffle :: (ChainComplex a) => a -> [Basis a] -> [Basis a] -> Combination [Basis a]
shuffle c [] [] = singleComb []
shuffle c as [] = singleComb as
shuffle c [] bs = singleComb bs
shuffle c (a : as) (b : bs) =
  mapMonotonic (a :) (shuffle c as (b : bs))
    + kozulRule eps (mapMonotonic (b :) (shuffle c (a : as) bs))
  where
    eps = (1 + degree c b) * (length (a : as) + sum (degree c <$> (a : as)))

instance
  (CommAlgebra a, AugmentedAlgebra a, ConnectedChainComplex a) =>
  Algebra (Bar a)
  where
  unitMor _ = basisMorphism (const [])
  muMor (Bar a) = Morphism 0 (uncurry (shuffle a))

instance
  (CommAlgebra a, AugmentedAlgebra a, ConnectedChainComplex a) =>
  AugmentedAlgebra (Bar a)
  where
  augmentationMor _ = Morphism 0 $ \word ->
    if null word then singleComb () else zeroCombination

instance
  (CommAlgebra a, AugmentedAlgebra a, ConnectedChainComplex a) =>
  CommAlgebra (Bar a)

barFunc ::
  ( AugmentedAlgebra a,
    AugmentedAlgebra b,
    ConnectedChainComplex a,
    ConnectedChainComplex b
  ) =>
  Morphism a b ->
  Morphism (Bar a) (Bar b)
barFunc (Morphism deg f) = Morphism deg (traverseCombination f)

horizPerturbation ::
  (AugmentedAlgebra a, ConnectedChainComplex a) =>
  a ->
  Morphism (BarTensor a) (BarTensor a)
horizPerturbation a = Morphism (-1) $ onBibasis $ hdiff (Bar a)

asBarReduction ::
  (AugmentedAlgebra a, ConnectedChainComplex a) =>
  Reduction x (Perturbed (BarTensor a)) ->
  Reduction x (Bar a)
asBarReduction = sameBasisReduction

augmentationIdealReduction ::
  (ChainComplex a, ChainComplex b) =>
  Reduction a b ->
  Reduction (AugmentationIdeal a) (AugmentationIdeal b)
augmentationIdealReduction = sameBasisReduction

-- | Lift a reduction to the unperturbed tensor algebras underlying Bar.
barTensorReduction ::
  (ChainComplex a, ChainComplex b) =>
  a ->
  b ->
  Reduction a b ->
  Reduction (BarTensor a) (BarTensor b)
barTensorReduction a b reduction =
  tensorAlgReduction
    (AugmentationIdeal a)
    (AugmentationIdeal b)
    (augmentationIdealReduction reduction)

barEquiv ::
  (AugmentedAlgebra a, ConnectedChainComplex a, ChainComplex b) =>
  Equivalence a b ->
  Equivalence (Bar a) (Perturbed (BarTensor b))
barEquiv (Equivalence a l x r b) = Equivalence (Bar a) (asBarReduction newl) newx newr newb
  where
    (newx, _, newl) =
      perturbBottom
        (barTensor x)
        (barTensor a)
        (barTensorReduction x a l)
        (horizPerturbation a)
    (_, newb, newr) =
      perturb
        (barTensor x)
        (barTensor b)
        (barTensorReduction x b r)
        (perturbedDiff newx)

-- TODO: universal twisting cochain a -> Bar a (should be same as the one induced by the twist on Wbar)
