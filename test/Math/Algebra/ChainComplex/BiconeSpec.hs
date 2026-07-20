module Math.Algebra.ChainComplex.BiconeSpec where

import Test.Hspec

import Math.Algebra.ChainComplex
import Math.Algebra.ChainComplex.Bicone
import Math.Algebra.ChainComplex.Disk
import Math.Algebra.ChainComplex.Reduction

import qualified Math.Algebra.ChainComplex.Properties as ChainComplexProperties
import qualified Math.Algebra.ChainComplex.Reduction.Properties as ReductionProperties

spec :: Spec
spec = describe "bicone" $ do
  let left = Disk 1
      right = Disk 2
      leftReduction = diskReduction left
      rightReduction = diskReduction right
      bicone = Bicone left () right (reductionF leftReduction) (reductionF rightReduction)
      biconeBasis = [-1 .. 3] >>= basis bicone
      leftBasis = [0 .. 3] >>= basis left
      rightBasis = [0 .. 3] >>= basis right

  describe "chain complex" $
    ChainComplexProperties.checkChainConditionOn bicone "bicone" biconeBasis

  describe "left projection reduction" $
    ReductionProperties.checkOn
      bicone
      left
      biconeBasis
      leftBasis
      (projRedLeft leftReduction rightReduction)

  describe "right projection reduction" $
    ReductionProperties.checkOn
      bicone
      right
      biconeBasis
      rightBasis
      (projRedRight leftReduction rightReduction)
