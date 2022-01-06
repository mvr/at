{-# LANGUAGE FlexibleContexts #-}
module Math.ValueCategory.Abelian where

import Math.ValueCategory
import Math.ValueCategory.Additive ( AdditiveCategory(zero) )

class AdditiveCategory a => AbelianCategory a where
  kernel       :: Arrow a -> Arrow a
  -- Given f : A -> B, f' : A' -> B' and phi : A -> A' such that TODO, this is the induced map ker f -> ker f'
  kernelArrow :: Arrow a -> Arrow a -> Arrow a -> Arrow a

  cokernel       :: Arrow a -> Arrow a
  -- Given f : A -> B, f' : A' -> B' and B -> B' such that TODO, this is the induced map coker f -> coker f'
  cokernelArrow :: Arrow a -> Arrow a -> Arrow a -> Arrow a

  {-# MINIMAL kernel, kernelArrow,
              cokernel, cokernelArrow #-}

kernelObject :: AbelianCategory a => Arrow a -> a
kernelObject = domain . kernel

cokernelObject :: AbelianCategory a => Arrow a -> a
cokernelObject = codomain . cokernel

imageObject :: (AbelianCategory a) => Arrow a -> a
imageObject = kernelObject . cokernel

image :: (AbelianCategory a) => Arrow a -> Arrow a
image = kernel . cokernel

imageArrow :: AbelianCategory a => Arrow a -> Arrow a -> Arrow a -> Arrow a
imageArrow f g phi = kernelArrow (cokernel f) (cokernel g) (cokernelArrow f g phi)

coimageObject :: (AbelianCategory a) => Arrow a -> a
coimageObject = cokernelObject . kernel

coimage :: (AbelianCategory a) => Arrow a -> Arrow a
coimage = cokernel . kernel

isInjective :: (Eq a, AbelianCategory a) => Arrow a -> Bool
isInjective f = kernelObject f == zero

isSurjective :: (Eq a, AbelianCategory a) => Arrow a -> Bool
isSurjective f = cokernelObject f == zero

isZeroArrow :: (Eq a, AbelianCategory a) => Arrow a -> Bool
isZeroArrow f = imageObject f == zero

-- Arrows assumed to be composable, with gf = 0
homology :: (AbelianCategory a) => Arrow a -> Arrow a -> a
homology f g = imageObject (cokernel f <> kernel g)

isExact :: (Eq a, AbelianCategory a) => Arrow a -> Arrow a -> Bool
isExact f g = homology f g == zero

isExactSequence :: (Eq a, AbelianCategory a) => [Arrow a] -> Bool
isExactSequence [] = True
isExactSequence [x] = True
isExactSequence (x:y:xs) = isExact x y && isExactSequence (y:xs)
