{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
module AbelianCategoryProperties where

import Test.Hspec
import Test.QuickCheck
import Data.Proxy

import Math.ValueCategory
import Math.ValueCategory.Abelian

spec :: forall c. (AbelianCategory c,
                   Eq c, Eq (Morphism c),
                   Show c, Show (Morphism c),
                   Arbitrary c, Arbitrary (Morphism c)
                  ) => Proxy c -> Spec
spec p = do
  describe "kernel" $ do
    it "has domain kernelObject" $ property $
      \(f :: Morphism c) -> domain (kernel f) == kernelObject f
    it "has codomain the domain of f" $ property $
      \(f :: Morphism c) -> codomain (kernel f) == domain f

  describe "kernelObject" $ do
    it "of identity is zero" $ property $
      \(a :: c) -> kernelObject (vid a) == zero
    it "of zero map is domain" $ property $
      \(a :: c, b) -> kernelObject (zeroMorphism a b) == a

  describe "kernelMorphism" $ do
    it "induced by identity is identity" $ property $
      \(l :: Morphism c) -> kernelMorphism l l (vid $ domain l) == (vid $ kernelObject l)

  describe "cokernel" $ do
    it "has domain the codomain of f" $ property $
      \(f :: Morphism c) -> domain (cokernel f) == codomain f
    it "has codomain cokernelObject" $ property $
      \(f :: Morphism c) -> codomain (cokernel f) == cokernelObject f

  describe "cokernelObject" $ do
    it "of identity is zero" $ property $
      \(a :: c) -> cokernelObject (vid a) == zero
    it "of zero map is codomain" $ property $
      \(a :: c, b) -> cokernelObject (zeroMorphism a b) == b

  describe "cokernelMorphism" $ do
    it "induced by identity is identity" $ property $
      \(l :: Morphism c) -> cokernelMorphism l l (vid $ codomain l) == (vid $ cokernelObject l)

  describe "imageObject" $ do
    it "of identity is itself" $ property $
      \(a :: c) -> imageObject (vid a) == a
    it "of zero map is zero" $ property $
      \(a :: c, b) -> imageObject (zeroMorphism a b) == zero

  describe "image == coimage" $ do
    it "is true" $ property $
      \(f :: Morphism c) -> imageObject f == coimageObject f
