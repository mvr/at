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

import ArbitraryMatrix()

spec = do
  describe "isoClass" $ do
    it "survives class -> group -> class" $ property $  do
      \(a :: IsoClass) -> isoClass (fromIsoClass a) == a
    it "survives group -> class -> group" $ property $ do
      \(a :: AbGroup) -> fromIsoClass (isoClass a) == a

  describe "kernelObject" $ do
    it "of identity is zero" $ property $
      \m -> kernel (vid (fromPresentation m)) == zero
    it "of zero map is domain" $ property $
      \(m1, m2) -> let a = fromPresentation m1
                       b = fromPresentation m2 in
                   kernel (zeroMorphism a b) == a

  describe "cokernel" $ do
    it "of identity is zero" $ property $
      \m -> cokernel (vid (fromPresentation m)) == zero

    it "of zero map is codomain" $ property $
      \(m1, m2) -> let a = fromPresentation m1
                       b = fromPresentation m2 in
                   cokernel (zeroMorphism a b) == b

  describe "image" $ do
    it "of identity is itself" $ property $
      \m -> image (vid (fromPresentation m)) == fromPresentation m

    it "of zero map is zero" $ property $
      \(m1, m2) -> let a = fromPresentation m1
                       b = fromPresentation m2 in
                   image (zeroMorphism a b) == zero

  describe "homology" $ do
    it "of test 1 is correct" $ do
      let zmod2 = fromPresentation $ M.fromList 1 1 [2]
          times2 = morphismFromReducedMatrix zmod2 zmod2 (M.fromList 1 1 [2])
        in homology times2 times2 `shouldBe` zmod2

    it "of test 2 is correct" $ do
      let zmod4 = fromPresentation $ M.fromList 1 1 [4]
          times2 = morphismFromReducedMatrix zmod4 zmod4 (M.fromList 1 1 [2])
        in homology times2 times2 `shouldBe` zero
