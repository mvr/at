module Math.ValueCategory.Abelian where

import Math.ValueCategory

class ValueCategory ob => AbelianCategory ob where
  zero     :: ob
  toZero   :: ob -> Morphism ob
  toZero a = zeroMorphism a zero
  fromZero :: ob -> Morphism ob
  fromZero b = zeroMorphism zero b

  zeroMorphism :: ob -> ob -> Morphism ob
  zeroMorphism a b = fromZero b .* toZero a

  kernel         :: Morphism ob -> ob
  kernelMorphism :: Morphism ob -> Morphism ob

  cokernel         :: Morphism ob -> ob
  cokernelMorphism :: Morphism ob -> Morphism ob

image :: (AbelianCategory a) => Morphism a -> a
image = kernel . cokernelMorphism

imageMorphism :: (AbelianCategory a) => Morphism a -> Morphism a
imageMorphism = kernelMorphism . cokernelMorphism

coimage :: (AbelianCategory a) => Morphism a -> a
coimage = cokernel . kernelMorphism

coimageMorphism :: (AbelianCategory a) => Morphism a -> Morphism a
coimageMorphism = cokernelMorphism . kernelMorphism

-- Morphisms assumed to be composable, with gf = 0
homology :: (AbelianCategory a) => Morphism a -> Morphism a -> a
homology f g = image (cokernelMorphism f .* kernelMorphism g)

isExact :: (Eq a, AbelianCategory a) => Morphism a -> Morphism a -> Bool
isExact f g = homology f g == zero
