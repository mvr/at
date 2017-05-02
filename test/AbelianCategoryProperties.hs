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
  describe "kernelObject" $ do
    it "of identity is zero" $ property $
      \(a :: c) -> kernelObject (vid a) == zero
    it "of zero map is domain" $ property $
      \(a :: c, b) -> kernelObject (zeroMorphism a b) == a

  describe "cokernelObject" $ do
    it "of identity is zero" $ property $
      \(a :: c) -> cokernelObject (vid a) == zero

    it "of zero map is codomain" $ property $
      \(a :: c, b) -> cokernelObject (zeroMorphism a b) == b

  describe "cokernelMorphism" $ do
    it "induced by identity is identity" $ property $
      \(l :: Morphism c) -> cokernelMorphism l l (vid $ codomain l) == (vid $ cokernelObject l)

  describe "kernelMorphism" $ do
    it "induced by identity is identity" $ property $
      \(l :: Morphism c) -> kernelMorphism l l (vid $ domain l) == (vid $ kernelObject l)

  describe "imageObject" $ do
    it "of identity is itself" $ property $
      \(a :: c) -> imageObject (vid a) == a
    it "of zero map is zero" $ property $
      \(a :: c, b) -> imageObject (zeroMorphism a b) == zero
