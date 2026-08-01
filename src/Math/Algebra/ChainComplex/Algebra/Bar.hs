{-# LANGUAGE UndecidableInstances #-}

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
-- Alg(Z) -Bar-> biCh(Z) -Tot-> Ch(Z)
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

-- To implement the action of `Bar` on reductions, we need a
-- `TensorAlgebra` functor, which only uses the vertical differentials
-- of the Bar bicomplex. Really, we are computing the tensor algebra
-- of the suspension of the original `a`.
-- TODO: this could be moved to its own file

newtype TensorSusp a = TensorSusp a

instance ChainComplex a => Bicomplex (TensorSusp a) where
  type Bibasis (TensorSusp a) = [Basis a]

  isBibasis (TensorSusp a) bs = all (\b -> degree a b /= 0) bs && all (isBasis a) bs

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
  diff (TensorSusp a) = Morphism (-1) (onBasis (diff (Tot (TensorSusp a))))

tensorAlgFunc ::
  (ChainComplex a, ChainComplex b) =>
  Morphism a b ->
  Morphism (TensorSusp a) (TensorSusp b)
tensorAlgFunc (Morphism deg f) = Morphism deg (traverseCombination f)

instance FiniteType a => Bi.FiniteType (TensorSusp a) where
  bibasis (TensorSusp a) (hd, vd) = go vd hd
    where
      go 0 0 = [[]]
      go i d | d <= 0 = []
      go i d = do
        j <- [1 .. d] -- Degree 0 basis elements are deliberately excluded
        b <- basis a j
        rest <- go (i - 1) (d - j)
        return (b : rest)

instance FiniteType a => FiniteType (TensorSusp a) where
  basis (TensorSusp a) = basis (Tot (TensorSusp a))

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

instance Algebra a => Bicomplex (Bar a) where
  type Bibasis (Bar a) = [Basis a]

  isBibasis (Bar a) = isBibasis (TensorSusp a)
  bidegree (Bar a) = bidegree (TensorSusp a)
  vdiff (Bar a) =
    Bimorphism (Bidegree (0, -1)) (onBibasis (vdiff (TensorSusp a)))

  hdiff (Bar a) = Bimorphism (Bidegree (-1, 0)) go
    where
      go :: [Basis a] -> Combination [Basis a]
      go [] = 0
      go [b1] = 0
      go (b1 : b2 : bs) = kozulRule (degree a b1 + 1) (mapMonotonic (: bs) (muMor a `onBasis` (b1, b2)) + mapMonotonic (b1 :) (go (b2 : bs)))

instance (Algebra a, FiniteType a) => Bi.FiniteType (Bar a) where
  bibasis (Bar a) = bibasis (TensorSusp a)

instance Algebra a => ChainComplex (Bar a) where
  type Basis (Bar a) = [Basis a]
  isBasis (Bar a) = isBasis (Tot (Bar a))
  degree (Bar a) = degree (Tot (Bar a))
  diff (Bar a) = Morphism (-1) (onBasis (diff (Tot (Bar a))))

instance (Algebra a, FiniteType a) => FiniteType (Bar a) where
  basis (Bar a) = basis (TensorSusp a)

shuffle :: (ChainComplex a) => a -> [Basis a] -> [Basis a] -> Combination [Basis a]
shuffle c [] [] = singleComb []
shuffle c as [] = singleComb as
shuffle c [] bs = singleComb bs
shuffle c (a : as) (b : bs) =
  mapMonotonic (a :) (shuffle c as (b : bs))
    + kozulRule eps (mapMonotonic (b :) (shuffle c (a : as) bs))
  where
    eps = (1 + degree c b) * (length (a : as) + sum (degree c <$> (a : as)))

instance (CommAlgebra a) => Algebra (Bar a) where
  unitMor _ = basisMorphism (const [])
  muMor (Bar a) = Morphism 0 (uncurry (shuffle a))

instance (CommAlgebra a) => CommAlgebra (Bar a)

barFunc ::
  (ChainComplex a, ChainComplex b) =>
  Morphism a b ->
  Morphism (Bar a) (Bar b)
barFunc (Morphism deg f) = Morphism deg (traverseCombination f)

horizPerturbation :: (Algebra a) => a -> Morphism (TensorSusp a) (TensorSusp a)
horizPerturbation a = Morphism (-1) $ onBibasis $ hdiff (Bar a)

asBarReduction :: Algebra a => Reduction x (Perturbed (TensorSusp a)) -> Reduction x (Bar a)
asBarReduction (Reduction (Morphism fd f) (Morphism gd g) h) =
  Reduction (Morphism fd f) (Morphism gd g) h

barEquiv ::
  (Algebra a, ChainComplex b) =>
  Equivalence a b ->
  Equivalence (Bar a) (Perturbed (TensorSusp b))
barEquiv (Equivalence a l x r b) = Equivalence (Bar a) (asBarReduction newl) newx newr newb
  where
    (newx, _, newl) = perturbBottom (TensorSusp x) (TensorSusp a) (tensorAlgReduction x a l) (horizPerturbation a)
    (_, newb, newr) = perturb (TensorSusp x) (TensorSusp b) (tensorAlgReduction x b r) (perturbedDiff newx)

-- TODO: universal twisting cochain a -> Bar a (should be same as the one induced by the twist on Wbar)
