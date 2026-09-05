module Math.Algebra.ChainComplex.Algebra.BarSpec where

import qualified Control.Category.Constrained as Constrained
import Control.Exception (evaluate)
import Control.Monad (forM_)
import Test.Hspec

import Math.Algebra.Bicomplex hiding (FiniteType)
import qualified Math.Algebra.ChainComplex as CC
import Math.Algebra.ChainComplex.Algebra.Bar
import Math.Algebra.ChainComplex.Disk
import Math.Algebra.Combination (singleComb)
import Math.Algebra.Group
import Math.Topology.SGrp.WbarDiscrete
import Math.Topology.SSet.NChains

import qualified Math.Algebra.Bicomplex.Properties as BicomplexProperties
import qualified Math.Algebra.ChainComplex.Algebra.Properties as AlgebraProperties
import qualified Math.Algebra.ChainComplex.Properties as ChainComplexProperties
import qualified Math.Algebra.ChainComplex.Reduction.Properties as ReductionProperties

data NegativeLine = NegativeLine

instance CC.ChainComplex NegativeLine where
  type Basis NegativeLine = ()
  degree _ _ = -1
  diff _ = CC.morphismZeroOfDeg (-1)

instance CC.FiniteType NegativeLine where
  basis _ (-1) = [()]
  basis _ _ = []

-- Requiring the class here also checks that the instance is available
-- without a connectedness assumption on the original complex.
oneReducedDimensions :: (CC.OneReducedChainComplex a, CC.FiniteType a) => a -> [Int]
oneReducedDimensions a = fmap (CC.dim a) [-1 .. 2]

spec :: Spec
spec = do
  describe "Bar tensor truncation" $ do
    let t = BarTensor (Disk 1)

    ChainComplexProperties.checkChainConditionOn t [[], [DiskInterior]]

    it "discards degree-zero boundaries" $
      CC.diff t `CC.onBasis` [DiskInterior] `shouldBe` 0

    it "excludes negative-degree generators" $
      CC.isBasis (BarTensor NegativeLine) [()] `shouldBe` False

    it "has lower bound zero without requiring a bound on the input" $
      CC.lowerBound (BarTensor NegativeLine) `shouldBe` 0

  describe "Bar tensor algebra" $ do
    it "is bounded below by zero" $
      CC.lowerBound (BarTensor (Disk 2)) `shouldBe` 0

    it "is one-reduced even for an input with multiple degree-zero generators" $
      oneReducedDimensions (BarTensor (Disk 1)) `shouldBe` [0, 1, 0, 1]

    it "is one-reduced even for a negative-degree input" $
      oneReducedDimensions (BarTensor NegativeLine) `shouldBe` [0, 1, 0, 0]

    it "has no words in negative bidegrees" $
      forM_ [(-1, 0), (-1, -1), (0, -1), (1, -1)] $ \d ->
        bibasis (BarTensor (Disk 2)) d `shouldBe` []

    it "includes the empty word only at bidegree (0,0)" $ do
      bibasis (BarTensor (Disk 2)) (0, 0) `shouldBe` [[]]
      bibasis (BarTensor (Disk 2)) (0, 1) `shouldBe` []

    AlgebraProperties.check 4 (BarTensor (Disk 2))
    AlgebraProperties.checkAugmented 4 (BarTensor (Disk 2))

  it "restricts Bar to the augmentation ideal" $
    CC.isBasis (BarTensor (Disk 2)) [DiskBase] `shouldBe` False

  describe "Bar tensor reduction" $
    ReductionProperties.check
      10
      (BarTensor (Disk 2))
      (BarTensor ())
      (barTensorReduction (Disk 2) (diskReduction (Disk 2)))

  describe "Bar shuffle" $ do
    let a = Disk 2

    it "uses the empty word as a unit, including on itself" $
      forM_ [[], [DiskBoundary], [DiskInterior, DiskBoundary]] $ \w -> do
        shuffle a [] w `shouldBe` singleComb w
        shuffle a w [] `shouldBe` singleComb w

    it "cancels the square of an odd suspended generator" $
      shuffle a [DiskInterior] [DiskInterior] `shouldBe` 0

    it "introduces no sign when crossing an even suspended generator" $
      shuffle a [DiskBoundary] [DiskInterior]
        `shouldBe` singleComb [DiskBoundary, DiskInterior] + singleComb [DiskInterior, DiskBoundary]

    it "uses the suspended degree of the whole remaining word" $
      shuffle a [DiskInterior, DiskBoundary] [DiskInterior]
        `shouldBe` singleComb [DiskInterior, DiskBoundary, DiskInterior]

  describe "Bar functor" $ do
    let a = Bar (BarTensor (Disk 2))
        f = barFunc (Constrained.id :: CC.Morphism (BarTensor Disk) (BarTensor Disk))

    it "preserves identity" $
      f `ChainComplexProperties.isIdOnAll` ([0 .. 6] >>= CC.basis a)

    it "rejects nonzero degrees" $
      forM_ [-1, 1] $ \d ->
        evaluate (barFunc (CC.morphismZeroOfDeg d :: CC.Morphism (BarTensor Disk) (BarTensor Disk)))
          `shouldThrow` errorCall "tensorAlgebraFunc: expected a degree-zero morphism"

  describe "Bar" $ do
    let a = Bar (NChains (WbarDiscrete (Zmod 3)))
    describe "is a bicomplex" $ do
      let as = do
            h <- [0 .. 5]
            v <- [0 .. 5]
            bibasis a (h, v)
      BicomplexProperties.checkChainConditions a as
    describe "is a chain complex" $
      ChainComplexProperties.checkChainConditionOn a ([0 .. 5] >>= CC.basis a)
    describe "is augmented" $
      AlgebraProperties.checkAugmented 5 a

  describe "BarBar" $ do
    let a = Bar (Bar (NChains (WbarDiscrete (Zmod 3))))
    describe "is a bicomplex" $ do
      let as = do
            h <- [0 .. 6]
            v <- [0 .. 6]
            bibasis a (h, v)
      BicomplexProperties.checkChainConditions a as
    describe "is a chain complex" $
      ChainComplexProperties.checkChainConditionOn a ([0 .. 6] >>= CC.basis a)
