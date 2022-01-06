-- | A monoid object in chain complexes, also known as a DG-algebra.
module Math.Algebra.ChainComplex.Algebra where

import Math.Algebra.ChainComplex
import Math.Algebra.ChainComplex.Tensor

class ChainComplex a => Algebra a where
  unitMor :: a -> Morphism () a -- This is just an element
  muMor :: a -> Morphism (Tensor a a) a
