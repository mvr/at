module Math.Algebra.AbGroupPresSpec where

import Control.Monad (forM_)
import Data.Proxy
import Test.Hspec
import Test.QuickCheck

import qualified Data.Matrix as M

import Math.Algebra.AbGroupPres
import Math.Algebra.AbGroupPres.IsoClass
import Math.ValueCategory
import Math.ValueCategory.Abelian
import Math.ValueCategory.Additive

import qualified Math.ValueCategory.Abelian.Properties as AbelianCategoryProperties
import TestSupport.ArbitraryInstances ()

spec :: Spec
spec = do
  describe "Abelian category problems for AbGroup" $
    AbelianCategoryProperties.spec (Proxy @AbGroupPres)

  describe "AbGroup" $ do
    describe "presentation coordinates" $ do
      it "uses no dummy generators or relations for zero and free groups" $
        forM_ [0 .. 3] $ \n -> do
          let p = freeAbGroup (fromIntegral n)
          presentation p `shouldBe` M.zero n 0
          reduced p `shouldBe` M.zero n 0
          toReduced p `shouldBe` M.identity n
          fromReduced p `shouldBe` M.identity n
          length (indGenerators p) `shouldBe` n

      it "reduces empty and all-zero presentations without padding" $
        forM_ [0 .. 3] $ \r -> forM_ [0 .. 3] $ \c -> do
          let p = fromPresentation (M.zero r c)
          p `shouldBe` freeAbGroup (fromIntegral r)
          reduced p `shouldBe` M.zero r 0
          toReduced p `shouldBe` M.identity r
          fromReduced p `shouldBe` M.identity r

      it "removes every killed generator, even in a rectangular presentation" $
        forM_ [(0, 0), (1, 1), (2, 3), (3, 2)] $ \(r, c) -> do
          let p = fromPresentation $ M.setSize 0 r c (M.identity (min r c))
              n = r - min r c
          p `shouldBe` freeAbGroup (fromIntegral n)
          reduced p `shouldBe` M.zero n 0
          (M.nrows $ fromReduced p, M.ncols $ fromReduced p) `shouldBe` (r, n)
          (M.nrows $ toReduced p, M.ncols $ toReduced p) `shouldBe` (n, r)
          toReduced p * fromReduced p `shouldBe` M.identity n

      it "removes killed generators from lifts when every relation is a unit" $ do
        let p = fromPresentation $ M.fromList 3 1 [2, 3, 4]
        p `shouldBe` freeAbGroup 2
        (M.nrows $ fromReduced p, M.ncols $ fromReduced p) `shouldBe` (3, 2)
        (M.nrows $ toReduced p, M.ncols $ toReduced p) `shouldBe` (2, 3)
        toReduced p * fromReduced p `shouldBe` M.identity 2
        toReduced p * presentation p `shouldBe` M.zero 2 1

    describe "normaliseElt" $ do
      it "represents the zero element of the trivial group by a 0-by-1 column" $
        eltVector (normaliseElt zero (M.zero 0 1)) `shouldBe` M.zero 0 1

      it "preserves free coordinates" $
        eltVector (normaliseElt (freeAbGroup 2) (M.fromList 2 1 [7, -3]))
          `shouldBe` M.fromList 2 1 [7, -3]

      it "reduces coefficients modulo the relation, not the reverse" $
        eltVector (normaliseElt (fromPresentation $ M.fromList 1 1 [4]) (M.fromList 1 1 [9]))
          `shouldBe` M.fromList 1 1 [1]

      it "normalises negative torsion coefficients" $
        eltVector (normaliseElt (fromPresentation $ M.fromList 1 1 [4]) (M.fromList 1 1 [-1]))
          `shouldBe` M.fromList 1 1 [3]

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
      it "agrees for trivial groups with and without redundant generators" $ do
        let p = fromPresentation (M.identity 2)
        vid p `shouldBe` vid (zero :: AbGroupPres)
        vid p `shouldBe` zeroArrow p p

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
