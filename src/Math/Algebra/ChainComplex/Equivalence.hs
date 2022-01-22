-- | A strong chain equivalence is a span of reductions.
module Math.Algebra.ChainComplex.Equivalence where

import Control.Category.Constrained
import Math.Algebra.ChainComplex
import Math.Algebra.ChainComplex.Bicone
import Math.Algebra.ChainComplex.Reduction
import Prelude hiding (id, (.))

data Equivalence a b = forall c.
  (ChainComplex c, Eq (Basis c)) =>
  Equivalence
  { equivLeft :: a,
    equivLeftRed :: Reduction c a,
    equivApex :: c,
    equivRightRed :: Reduction c b,
    equivRight :: b
  }

instance Category Equivalence where
  type Object Equivalence a = (ChainComplex a, Eq (Basis a))

  id = error "Equivalence: id"
  (Equivalence b1 l1 x1 r1 c1) . (Equivalence a2 l2 x2 r2 b2) =
    Equivalence
      a2
      (l2 . projRedLeft r2 l1)
      (Bicone x2 b1 x1 (reductionF r2) (reductionF l1))
      (r1 . projRedRight r2 l1)
      c1

idEquiv :: (ChainComplex a) => a -> Equivalence a a
idEquiv a = Equivalence a id a id a

isoToEquiv :: (ChainComplex a) => a -> b -> Morphism a b -> Morphism b a -> Equivalence a b
isoToEquiv a b f g = Equivalence a id a (isoToReduction f g) b

fromRedLeft :: ChainComplex a => a -> b -> Reduction a b -> Equivalence a b
fromRedLeft a b r = Equivalence a id a r b

composeLeft :: (Eq (Basis a), Eq (Basis a')) => a' -> Reduction a a' -> Equivalence a b -> Equivalence a' b
composeLeft a' l' (Equivalence a l x r b) = Equivalence a' (l' . l) x r b

perturbLeft ::
  (Eq (Basis a), Eq (Basis b)) =>
  Equivalence a b ->
  Morphism a a ->
  Equivalence (Perturbed a) (Perturbed b)
perturbLeft (Equivalence a l x r b) m =
  let (x'@(Perturbed _ m'), a', l') = perturbBottom x a l m
      (_, b', r') = perturb x b r m'
   in Equivalence a' l' x' r' b'

perturbRight ::
  (Eq (Basis a), Eq (Basis b)) =>
  Equivalence a b ->
  Morphism b b ->
  Equivalence (Perturbed a) (Perturbed b)
perturbRight (Equivalence a l x r b) m =
  let (x'@(Perturbed _ m'), b', r') = perturbBottom x b r m
      (_, a', l') = perturb x a l m'
   in Equivalence a' l' x' r' b'

-- equivComposeIso :: (Eq (Basis b), Eq (Basis c)) => (Morphism b c, Morphism c b) -> Equivalence a b -> Equivalence a c
-- equivComposeIso (p,q) (Equivalence x rl (Reduction f g h)) = Equivalence x rl (Reduction (p . f) (g . q) h)
