module Math.Algebra.ChainComplex.ShiftSpec where

import Test.Hspec

import Math.Algebra.ChainComplex
import Math.Algebra.ChainComplex.Disk
import Math.Algebra.ChainComplex.Shift
import Math.Algebra.Combination

import qualified Math.Algebra.ChainComplex.Properties as ChainComplexProperties
import qualified Math.Algebra.ChainComplex.Reduction.Properties as ReductionProperties

spec :: Spec
spec = describe "chain-complex shift" $ do
  it "adds an arbitrary amount to degrees" $ do
    degree (Shift 3 (Disk 2)) DiskInterior `shouldBe` 5
    degree (Shift (-2) (Disk 2)) DiskInterior `shouldBe` 0

  it "negates the differential for odd shifts" $ do
    diff (Susp (Disk 2)) `onBasis` DiskInterior
      `shouldBe` -(singleComb DiskBoundary)
    diff (Desusp (Disk 2)) `onBasis` DiskInterior
      `shouldBe` -(singleComb DiskBoundary)

  it "preserves the differential for even shifts" $
    diff (Shift 2 (Disk 2)) `onBasis` DiskInterior
      `shouldBe` singleComb DiskBoundary

  ChainComplexProperties.checkChainCondition (Susp (Disk 2)) 3

  describe "shifted reduction" $
    ReductionProperties.checkOn
      (Shift 3 (Disk 2))
      (Shift 3 ())
      [DiskBase, DiskBoundary, DiskInterior]
      [()]
      (shiftReduction 3 (diskReduction (Disk 2)))
