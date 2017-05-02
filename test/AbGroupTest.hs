{-# LANGUAGE ScopedTypeVariables #-}
module AbGroupTest where

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
  describe "AbGroup" $ do
    describe "isoClass" $ do
      it "survives class -> group -> class" $ property $  do
        \(a :: IsoClass) -> isoClass (fromIsoClass a) == a
      it "survives group -> class -> group" $ property $ do
        \(a :: AbGroup) -> fromIsoClass (isoClass a) == a

    describe "homology" $ do
      it "of test 1 is correct" $ do
        let zmod2 = fromPresentation $ M.fromList 1 1 [2]
            times2 = morphismFromReducedMatrix zmod2 zmod2 (M.fromList 1 1 [2])
          in homology times2 times2 `shouldBe` zmod2

      it "of test 2 is correct" $ do
        let zmod4 = fromPresentation $ M.fromList 1 1 [4]
            times2 = morphismFromReducedMatrix zmod4 zmod4 (M.fromList 1 1 [2])
          in homology times2 times2 `shouldBe` zero
