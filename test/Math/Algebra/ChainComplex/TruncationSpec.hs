module Math.Algebra.ChainComplex.TruncationSpec where

import Test.Hspec

import Math.Algebra.ChainComplex
import Math.Algebra.ChainComplex.Disk
import Math.Algebra.ChainComplex.Shift (Desusp (..))
import Math.Algebra.ChainComplex.TensorAlgebra (TensorAlgebra (..))
import Math.Algebra.ChainComplex.Truncation
import Math.Algebra.Combination

import qualified Math.Algebra.ChainComplex.Properties as ChainComplexProperties

spec :: Spec
spec = describe "naive truncation" $ do
  let truncated = NaiveTruncation 1 (Disk 2)

  ChainComplexProperties.checkChainConditionOn
    truncated
    [DiskBoundary, DiskInterior]

  it "uses the cutoff as a lower bound" $
    lowerBound truncated `shouldBe` 1

  it "gives a lower bound even when the original complex is unbounded below" $
    lowerBound (NaiveTruncation (-2) (TensorAlgebra (Desusp ()))) `shouldBe` (-2)

  it "excludes generators below the cutoff" $ do
    isBasis truncated DiskBase `shouldBe` False
    basis truncated 0 `shouldBe` []

  it "retains generators at and above the cutoff" $ do
    basis truncated 1 `shouldBe` [DiskBoundary]
    basis truncated 2 `shouldBe` [DiskInterior]

  it "kills the differential out of the cutoff degree" $
    diff (NaiveTruncation 2 (Disk 2)) `onBasis` DiskInterior
      `shouldBe` zeroCombination

  it "retains differentials above the cutoff" $
    diff truncated `onBasis` DiskInterior
      `shouldBe` singleComb DiskBoundary
