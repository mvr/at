module Math.Algebra.ChainComplex.Algebra.BarSpec where

import Test.Hspec

import Math.Algebra.Bicomplex hiding (FiniteType)
import qualified Math.Algebra.ChainComplex as CC
import Math.Algebra.ChainComplex.Algebra.Bar
import Math.Algebra.ChainComplex.Disk
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

spec :: Spec
spec = do
  describe "unrestricted tensor suspension" $ do
    let tensorSusp = TensorSusp (Disk 2)

    ChainComplexProperties.checkChainConditionOn
      tensorSusp
      [[], [DiskBase], [DiskBoundary], [DiskInterior], [DiskInterior, DiskInterior]]

    it "allows degree-zero generators" $
      CC.degree tensorSusp [DiskBase] `shouldBe` 1

  describe "augmentation-ideal truncation" $ do
    let ideal = AugmentationIdeal (Disk 1)

    ChainComplexProperties.checkChainConditionOn
      ideal
      [DiskInterior]

    it "discards degree-zero boundaries" $
      CC.diff ideal `CC.onBasis` DiskInterior `shouldBe` 0

    it "excludes negative-degree generators" $
      CC.isBasis (AugmentationIdeal NegativeLine) () `shouldBe` False

  describe "tensor suspension algebra" $ do
    AlgebraProperties.check 4 (barTensor (Disk 2))
    AlgebraProperties.checkAugmented 4 (barTensor (Disk 2))

  it "restricts Bar to the augmentation ideal" $
    CC.isBasis (barTensor (Disk 2)) [DiskBase] `shouldBe` False

  describe "tensor algebra reduction signs" $
    ReductionProperties.check
      10
      (barTensor (Disk 2))
      (barTensor ())
      (barTensorReduction (Disk 2) () (diskReduction (Disk 2)))

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
