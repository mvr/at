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

  kernelObject :: Morphism ob -> ob
  kernel       :: Morphism ob -> Morphism ob
  -- Given f : A -> B, f' : A' -> B' and phi : A -> A' such that TODO, this is the induced map ker f -> ker f'
  kernelMorphism :: Morphism ob -> Morphism ob -> Morphism ob -> Morphism ob

  cokernelObject :: Morphism ob -> ob
  cokernel       :: Morphism ob -> Morphism ob
  -- Given f : A -> B, f' : A' -> B' and B -> B' such that TODO, this is the induced map coker f -> coker f'
  cokernelMorphism :: Morphism ob -> Morphism ob -> Morphism ob -> Morphism ob

imageObject :: (AbelianCategory a) => Morphism a -> a
imageObject = kernelObject . cokernel

image :: (AbelianCategory a) => Morphism a -> Morphism a
image = kernel . cokernel

coimageObject :: (AbelianCategory a) => Morphism a -> a
coimageObject = cokernelObject . kernel

coimage :: (AbelianCategory a) => Morphism a -> Morphism a
coimage = cokernel . kernel

-- Morphisms assumed to be composable, with gf = 0
homology :: (AbelianCategory a) => Morphism a -> Morphism a -> a
homology f g = imageObject (cokernel f .* kernel g)

isExact :: (Eq a, AbelianCategory a) => Morphism a -> Morphism a -> Bool
isExact f g = homology f g == zero

isExactSequence :: (Eq a, AbelianCategory a) => [Morphism a] -> Bool
isExactSequence [] = True
isExactSequence [x] = True
isExactSequence (x:y:xs) = isExact x y && isExactSequence (y:xs)
