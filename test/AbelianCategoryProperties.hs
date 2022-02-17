module AbelianCategoryProperties where

import Data.Proxy
import Test.Hspec
import Test.QuickCheck

import Math.ValueCategory
import Math.ValueCategory.Abelian
import Math.ValueCategory.Additive

spec ::
  forall c.
  ( AbelianCategory c,
    Eq c,
    Eq (Arrow c),
    Show c,
    Show (Arrow c),
    Arbitrary c,
    Arbitrary (Arrow c)
  ) =>
  Proxy c ->
  Spec
spec p = do
  describe "kernel" $ do
    it "has domain kernelObject" $
      property $
        \(f :: Arrow c) -> domain (kernel f) == kernelObject f
    it "has codomain the domain of f" $
      property $
        \(f :: Arrow c) -> codomain (kernel f) == domain f
    it "composes with f to zero" $
      property $
        \(f :: Arrow c) -> isZeroArrow (f <> kernel f)

  describe "kernelObject" $ do
    it "of identity is zero" $
      property $
        \(a :: c) -> kernelObject (vid a) == zero
    it "of zero map is domain" $
      property $
        \(a :: c, b) -> kernelObject (zeroArrow a b) == a

  describe "kernelMorphism" $ do
    it "induced by identity is identity" $
      property $
        \(l :: Arrow c) -> kernelArrow l l (vid $ domain l) == vid (kernelObject l)

  describe "cokernel" $ do
    it "has domain the codomain of f" $
      property $
        \(f :: Arrow c) -> domain (cokernel f) == codomain f
    it "has codomain cokernelObject" $
      property $
        \(f :: Arrow c) -> codomain (cokernel f) == cokernelObject f
    it "composes with f to zero" $
      property $
        \(f :: Arrow c) -> isZeroArrow (cokernel f <> f)

  describe "cokernelObject" $ do
    it "of identity is zero" $
      property $
        \(a :: c) -> cokernelObject (vid a) == zero
    it "of zero map is codomain" $
      property $
        \(a :: c, b) -> cokernelObject (zeroArrow a b) == b

  describe "cokernelMorphism" $ do
    it "induced by identity is identity" $
      property $
        \(l :: Arrow c) -> cokernelArrow l l (vid $ codomain l) == vid (cokernelObject l)

  describe "imageObject" $ do
    it "of identity is itself" $
      property $
        \(a :: c) -> imageObject (vid a) == a
    it "of zero map is zero" $
      property $
        \(a :: c, b) -> imageObject (zeroArrow a b) == zero

  describe "image == coimage" $ do
    it "is true" $
      property $
        \(f :: Arrow c) -> imageObject f == coimageObject f
