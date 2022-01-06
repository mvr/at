-- | A comonoid object in chain complexes, also known as a DG-coalgebra.
module Math.Algebra.ChainComplex.Coalgebra where

import Math.Algebra.ChainComplex
import Math.Algebra.ChainComplex.Tensor

class ChainComplex a => Coalgebra a where
  counitMor :: a -> Morphism a ()
  delMor :: a -> Morphism a (Tensor a a)
