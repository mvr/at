-- | A monoid object in chain complexes, also known as a DG-algebra.
module Math.Algebra.ChainComplex.Algebra where

import Math.Algebra.ChainComplex
import Math.Algebra.ChainComplex.Tensor

class ChainComplex a => Algebra a where
  unitMor :: a -> Morphism () a -- This is just an element
  muMor :: a -> Morphism (Tensor a a) a

-- | An algebra with a chosen augmentation to the ground ring.
class Algebra a => AugmentedAlgebra a where
  augmentationMor :: a -> Morphism a ()

-- Can transfer algebra structure across reductions
class Algebra a => CommAlgebra a
