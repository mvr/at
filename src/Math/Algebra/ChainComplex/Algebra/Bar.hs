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

import Data.Coerce ( coerce )
import Control.Applicative ( Applicative(liftA2) )
import Control.Category.Constrained ((.))
import Prelude hiding ((.))

import Math.Algebra.Bicomplex hiding (FiniteType)
import qualified Math.Algebra.Bicomplex as Bi (FiniteType)
import Math.Algebra.ChainComplex
import Math.Algebra.ChainComplex.Algebra
import Math.Algebra.ChainComplex.Reduction
import Math.Algebra.ChainComplex.Equivalence
import Math.Algebra.Combination

-- To implement the action of `Bar` on reductions, we need a
-- `TensorAlgebra` functor, which only uses the vertical differentials
-- of the Bar bicomplex. Really, we are computing the tensor algebra
-- of the suspension of the original `a`.
-- TODO: this could be moved to its own file

newtype TensorSusp a = TensorSusp { unTensorSusp :: a }

newtype TensorSuspBibasis a = TensorSuspBibasis a
  deriving (Eq)
  deriving (Show) via a

newtype TensorSuspBasis a = TensorSuspBasis a
  deriving (Eq)
  deriving (Show) via a

instance ChainComplex a => Bicomplex (TensorSusp a) where
  type Bibasis (TensorSusp a) = TensorSuspBibasis [Basis a]

  isBibasis (TensorSusp a) (TensorSuspBibasis bs) = all (\b -> degree a b /= 0) bs && all (isBasis a) bs

  bidegree (TensorSusp a) (TensorSuspBibasis bs) = (length bs, sum (degree a <$> bs))

  vdiff (TensorSusp a) = Morphism (Bidegree (0, -1)) (coerce go)
    where
      go :: [Basis a] -> Combination [Basis a]
      go [] = 0
      go (b : bs) = fmap (: bs) (diff a `onBasis` b) + kozulRule (degree a b + 1) (fmap (b :) (go bs))

  hdiff _ = morphismZero

instance ChainComplex a => ChainComplex (TensorSusp a) where
  type Basis (TensorSusp a) = TensorSuspBasis (Basis (Tot (TensorSusp a)))
  degree (TensorSusp a) = coerce (degree (Tot (TensorSusp a)))
  diff (TensorSusp a) = coerce (diff (Tot (TensorSusp a)))

tensorAlgFunc ::
  (ChainComplex a, ChainComplex b) =>
  Morphism a b ->
  Morphism (TensorSusp a) (TensorSusp b)
tensorAlgFunc (Morphism deg f) = Morphism deg (coerce $ traverse @[] f)

instance (Algebra a, FiniteType a) => Bi.FiniteType (TensorSusp a) where
  bibasis (TensorSusp a) (hd, vd) = TensorSuspBibasis <$> go vd hd
    where
      go 0 0 = [[]]
      go i d | d <= 0 = []
      go i d = do
        j <- [1 .. d] -- Degree 0 basis elements are deliberately excluded
        b <- basis a j
        rest <- go (i - 1) (d - j)
        return (b : rest)

instance (Algebra a, FiniteType a) => FiniteType (TensorSusp a) where
  basis (TensorSusp a) i = TensorSuspBasis <$> basis (Tot (TensorSusp a)) i

verth :: (ChainComplex a) => Morphism a a -> Morphism a a -> [Basis a] -> Combination [Basis a]
verth h gf [] = 0
verth h gf (b:bs) = liftA2 (:) (h `onBasis` b) (coerce (tensorAlgFunc gf `onBasis` TensorSuspBasis (TotBasis (TensorSuspBibasis bs))))
                    + fmap (b:) (verth h gf bs)

tensorAlgReduction ::
  (ChainComplex a, ChainComplex b) =>
  a ->
  b ->
  Reduction a b ->
  Reduction (TensorSusp a) (TensorSusp b)
tensorAlgReduction a b r@(Reduction f g h) = Reduction (tensorAlgFunc f) (tensorAlgFunc g) (Morphism 1 $ coerce $ verth h (g . f))

newtype Bar a = Bar a

newtype BarBibasis a = BarBibasis a
  deriving (Eq)
  deriving (Show) via a

newtype BarBasis a = BarBasis a
  deriving (Eq)
  deriving (Show) via a

instance Algebra a => Bicomplex (Bar a) where
  type Bibasis (Bar a) = BarBibasis [Basis a]

  isBibasis (Bar a) = coerce (isBibasis (TensorSusp a))
  bidegree  (Bar a) = coerce (bidegree (TensorSusp a))
  vdiff     (Bar a) = coerce (vdiff (TensorSusp a))

  hdiff (Bar a) = Morphism (Bidegree (-1, 0)) (coerce go)
    where
      go :: [Basis a] -> Combination [Basis a]
      go [] = 0
      go [b1] = 0
      go (b1 : b2 : bs) = kozulRule (degree a b1 + 1) (fmap (: bs) (muMor a `onBasis` (b1, b2)) + fmap (b1 :) (go (b2 : bs)))

instance (Algebra a, FiniteType a) => Bi.FiniteType (Bar a) where
  bibasis (Bar a) = coerce (bibasis (TensorSusp a))

-- Can this be done using DerivingVia?
instance Algebra a => ChainComplex (Bar a) where
  type Basis (Bar a) = BarBasis (Basis (Tot (Bar a))) -- Reuse the same carrier Bar
  degree (Bar a) = coerce (degree (Tot (Bar a)))
  diff (Bar a) = coerce (diff (Tot (Bar a)))

instance (Algebra a, FiniteType a) => FiniteType (Bar a) where
  basis (Bar a) = coerce (basis (TensorSusp a))

shuffle :: (ChainComplex a) => a -> [Basis a] -> [Basis a] -> Combination [Basis a]
shuffle c [] [] = 0
shuffle c as [] = singleComb as
shuffle c [] bs = singleComb bs
shuffle c (a : as) (b : bs) =
  fmap (a :) (shuffle c as (b : bs))
    + kozulRule eps (fmap (b :) (shuffle c (a : as) bs))
  where
    eps = (1 + degree c b) * (length (a : as) + sum (degree c <$> (a : as)))

instance (CommAlgebra a) => Algebra (Bar a) where
  unitMor _ = fmapBasis (const (coerce @[Basis a] []))
  muMor (Bar a) = Morphism 0 $ coerce (uncurry (shuffle a))

instance (CommAlgebra a) => CommAlgebra (Bar a)

barFunc ::
  (ChainComplex a, ChainComplex b) =>
  Morphism a b ->
  Morphism (Bar a) (Bar b)
barFunc (Morphism deg f) = Morphism deg (coerce $ traverse @[] f)

horizPerturbation :: (Algebra a) => a -> Morphism (TensorSusp a) (TensorSusp a)
horizPerturbation a = Morphism (-1) $ coerce $ underlyingFunction $ hdiff (Bar a)

barEquiv ::
  (Algebra a, ChainComplex b) =>
  Equivalence a b ->
  Equivalence (Bar a) (Perturbed (TensorSusp b))
barEquiv (Equivalence a l x r b) = Equivalence (Bar (unTensorSusp (perturbedOrig newa))) (coerce newl) newx newr newb
  where (newx, newa, newl) = perturbBottom (TensorSusp x) (TensorSusp a) (tensorAlgReduction x a l) (horizPerturbation a)
        (_, newb, newr) = perturb (TensorSusp x) (TensorSusp b) (tensorAlgReduction x b r) (perturbedDiff newx)

-- TODO: universal twisting cochain a -> Bar a (should be same as the one induced by the twist on Wbar)
