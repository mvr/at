module Math.ValueCategory.Abelian where

import Math.ValueCategory

class ValueCategory ob => AbelianCategory ob where
  zero     :: ob
  toZero   :: ob -> Morphism ob
  fromZero :: ob -> Morphism ob

  kernel         :: Morphism ob -> ob
  kernelMorphism :: Morphism ob -> Morphism ob

  cokernel         :: Morphism ob -> ob
  cokernelMorphism :: Morphism ob -> Morphism ob
