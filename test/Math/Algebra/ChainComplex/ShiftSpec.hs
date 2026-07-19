module Math.Algebra.ChainComplex.ShiftSpec where

import Test.Hspec

import Math.Algebra.ChainComplex
import Math.Algebra.ChainComplex.Disk
import Math.Algebra.ChainComplex.Shift
import Math.Algebra.Combination

import qualified Math.Algebra.ChainComplex.Properties as ChainComplexProperties

spec :: Spec
spec = describe "chain-complex shift" $ do
  it "negates the shifted differential" $
    diff (Shift (Disk 2)) `onBasis` ShiftBasis DiskInterior
      `shouldBe` -(singleComb (ShiftBasis DiskBoundary))

  ChainComplexProperties.checkChainCondition (Shift (Disk 2)) 3
