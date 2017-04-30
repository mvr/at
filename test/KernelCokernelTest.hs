{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
module KernelCokernelTest where

import Test.Hspec
import Test.QuickCheck
import Debug.Trace

import Data.List (sort, dropWhileEnd)
import Data.Matrix (Matrix)
import qualified Data.Matrix as M
import qualified Data.Vector as V

import Math.Algebra.AbGroup
import Math.Algebra.AbGroup.IsoClass
import Math.ValueCategory
import Math.ValueCategory.Abelian

import ArbitraryInstances ()

spec = do
  describe "isoClass" $ do
    it "survives class -> group -> class" $ property $  do
      \(a :: IsoClass) -> isoClass (fromIsoClass a) == a
    it "survives group -> class -> group" $ property $ do
      \(a :: AbGroup) -> fromIsoClass (isoClass a) == a

  describe "kernelObject" $ do
    it "of identity is zero" $ property $
      \(a :: AbGroup) -> kernelObject (vid a) == zero
    it "of zero map is domain" $ property $
      \(a :: AbGroup, b) -> kernelObject (zeroMorphism a b) == a

  describe "cokernelObject" $ do
    it "of identity is zero" $ property $
      \(a :: AbGroup) -> cokernelObject (vid a) == zero

    it "of zero map is codomain" $ property $
      \(a :: AbGroup, b) -> cokernelObject (zeroMorphism a b) == b

  describe "cokernelMorphism" $ do
    it "induced by identity is identity" $ property $
      \(l :: AbMorphism) -> cokernelMorphism l l (vid $ codomain l) == (vid $ cokernelObject l)

  describe "imageObject" $ do
    it "of identity is itself" $ property $
      \(a :: AbGroup) -> imageObject (vid a) == a
    it "of zero map is zero" $ property $
      \(a :: AbGroup, b) -> imageObject (zeroMorphism a b) == zero

  describe "homology" $ do
    it "of test 1 is correct" $ do
      let zmod2 = fromPresentation $ M.fromList 1 1 [2]
          times2 = morphismFromReducedMatrix zmod2 zmod2 (M.fromList 1 1 [2])
        in homology times2 times2 `shouldBe` zmod2

    it "of test 2 is correct" $ do
      let zmod4 = fromPresentation $ M.fromList 1 1 [4]
          times2 = morphismFromReducedMatrix zmod4 zmod4 (M.fromList 1 1 [2])
        in homology times2 times2 `shouldBe` zero
