{-# LANGUAGE FlexibleContexts #-}
module Math.ValueCategory.Abelian where

import Math.ValueCategory

class ValueCategory a => AbelianCategory a where
  zero     :: a
  toZero   :: a -> Morphism a
  toZero a = zeroMorphism a zero
  fromZero :: a -> Morphism a
  fromZero b = zeroMorphism zero b

  zeroMorphism :: a -> a -> Morphism a
  zeroMorphism a b = fromZero b .* toZero a

  addMorphisms :: Morphism a -> Morphism a -> Morphism a
  subtractMorphisms :: Morphism a -> Morphism a -> Morphism a
  negateMorphism :: Morphism a -> Morphism a

  kernelObject :: Morphism a -> a
  kernel       :: Morphism a -> Morphism a
  -- Given f : A -> B, f' : A' -> B' and phi : A -> A' such that TODO, this is the induced map ker f -> ker f'
  kernelMorphism :: Morphism a -> Morphism a -> Morphism a -> Morphism a

  cokernelObject :: Morphism a -> a
  cokernel       :: Morphism a -> Morphism a
  -- Given f : A -> B, f' : A' -> B' and B -> B' such that TODO, this is the induced map coker f -> coker f'
  cokernelMorphism :: Morphism a -> Morphism a -> Morphism a -> Morphism a

imageObject :: (AbelianCategory a) => Morphism a -> a
imageObject = kernelObject . cokernel

image :: (AbelianCategory a) => Morphism a -> Morphism a
image = kernel . cokernel

imageMorphism :: AbelianCategory a => Morphism a -> Morphism a -> Morphism a -> Morphism a
imageMorphism f g phi = kernelMorphism (cokernel f) (cokernel g) (cokernelMorphism f g phi)

coimageObject :: (AbelianCategory a) => Morphism a -> a
coimageObject = cokernelObject . kernel

coimage :: (AbelianCategory a) => Morphism a -> Morphism a
coimage = cokernel . kernel

isInjective :: (Eq a, AbelianCategory a) => Morphism a -> Bool
isInjective f = kernelObject f == zero

isSurjective :: (Eq a, AbelianCategory a) => Morphism a -> Bool
isSurjective f = cokernelObject f == zero

-- Morphisms assumed to be composable, with gf = 0
homology :: (AbelianCategory a) => Morphism a -> Morphism a -> a
homology f g = imageObject (cokernel f .* kernel g)

isExact :: (Eq a, AbelianCategory a) => Morphism a -> Morphism a -> Bool
isExact f g = homology f g == zero

isExactSequence :: (Eq a, AbelianCategory a) => [Morphism a] -> Bool
isExactSequence [] = True
isExactSequence [x] = True
isExactSequence (x:y:xs) = isExact x y && isExactSequence (y:xs)
