-- | A strong chain equivalence is a span of reductions.
module Math.Algebra.ChainComplex.Equivalence where

import Prelude hiding (id, (.))
import Control.Category.Constrained

import Math.Algebra.ChainComplex
import Math.Algebra.ChainComplex.Bicone
import Math.Algebra.ChainComplex.Reduction

data Equivalence a b = forall c.
  (ChainComplex c, Eq (Basis c)) =>
  Equivalence
  { equivLeft :: Reduction c a,
    equivRight :: Reduction c b
  }

instance Category Equivalence where
  type Object Equivalence a = (ChainComplex a, Eq (Basis a))
  id = Equivalence id id
  (Equivalence l1 r1) . (Equivalence l2 r2) = Equivalence (l2 . redLeft r2 l1) (r1 . redRight r2 l1)

isoToEquiv :: (ChainComplex a, Eq (Basis a)) => Morphism a b -> Morphism b a -> Equivalence a b
isoToEquiv f g = Equivalence id (isoToReduction f g)

equivComposeIso :: (Eq (Basis b), Eq (Basis c)) => Equivalence a b -> Morphism b c -> Morphism c b -> Equivalence a c
equivComposeIso (Equivalence rl (Reduction f g h)) p q = Equivalence rl (Reduction (p . f) (g . q) h)
