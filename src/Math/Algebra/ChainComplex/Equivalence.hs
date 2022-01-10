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

  -- id = Equivalence id id
  (Equivalence b1 l1 x1 r1 c1) . (Equivalence a2 l2 x2 r2 b2) =
    Equivalence
      a2
      (l2 . redLeft r2 l1)
      (Bicone x2 b1 x1 (reductionF r2) (reductionF l1))
      (r1 . redRight r2 l1)
      c1

idEquiv :: (ChainComplex a, Eq (Basis a)) => a -> Equivalence a a
idEquiv a = Equivalence a id a id a

isoToEquiv :: (ChainComplex a, Eq (Basis a)) => a -> b -> Morphism a b -> Morphism b a -> Equivalence a b
isoToEquiv a b f g = Equivalence a id a (isoToReduction f g) b

-- equivComposeIso :: (Eq (Basis b), Eq (Basis c)) => (Morphism b c, Morphism c b) -> Equivalence a b -> Equivalence a c
-- equivComposeIso (p,q) (Equivalence x rl (Reduction f g h)) = Equivalence x rl (Reduction (p . f) (g . q) h)
