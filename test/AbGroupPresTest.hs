module AbGroupPresTest where

import Test.Hspec
import Test.QuickCheck

import qualified Data.Matrix as M

import Math.Algebra.AbGroupPres
import Math.Algebra.AbGroupPres.IsoClass
import Math.ValueCategory.Abelian
import Math.ValueCategory.Additive

import ArbitraryInstances ()

spec :: Spec
spec = do
  describe "AbGroup" $ do
    describe "isoClass" $ do
      it "survives class -> group -> class" $
        property $ do
          \(a :: IsoClass) -> isoClass (fromIsoClass a) == a
      it "survives group -> class -> group" $
        property $ do
          \(a :: AbGroupPres) -> fromIsoClass (isoClass a) == a

    describe "homology" $ do
      it "of test 1 is correct" $ do
        let zmod2 = fromPresentation $ M.fromList 1 1 [2]
            times2 = morphismFromReducedMatrix zmod2 zmod2 (M.fromList 1 1 [2])
         in homology times2 times2 `shouldBe` zmod2

      it "of test 2 is correct" $ do
        let zmod4 = fromPresentation $ M.fromList 1 1 [4]
            times2 = morphismFromReducedMatrix zmod4 zmod4 (M.fromList 1 1 [2])
         in homology times2 times2 `shouldBe` zero

    describe "morphism equality" $ do
      it "distinguishes arrows with different endpoints" $ do
        let zmod2 = fromPresentation $ M.fromList 1 1 [2]
            zmod3 = fromPresentation $ M.fromList 1 1 [3]
         in zeroArrow zmod2 zmod2 `shouldNotBe` zeroArrow zmod3 zmod2

      it "compares arrows through reduced presentations" $ do
        let zmod2 = fromPresentation $ M.fromList 1 1 [2]
            zmod2WithRedundantGenerator = fromPresentation $ M.fromList 2 2 [1, 0, 0, 2]
            identityMatrix = M.fromList 1 1 [1]
            f = morphismFromReducedMatrix zmod2 zmod2 identityMatrix
            g = morphismFromReducedMatrix zmod2WithRedundantGenerator zmod2WithRedundantGenerator identityMatrix
         in f `shouldBe` g
