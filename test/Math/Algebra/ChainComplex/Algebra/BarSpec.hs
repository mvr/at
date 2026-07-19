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
import qualified Math.Algebra.ChainComplex.Properties as ChainComplexProperties
import qualified Math.Algebra.ChainComplex.Reduction.Properties as ReductionProperties

spec :: Spec
spec = do
  describe "tensor algebra reduction signs" $
    ReductionProperties.check
      10
      (TensorSusp (Disk 2))
      (TensorSusp ())
      (tensorAlgReduction (Disk 2) () (diskReduction (Disk 2)))

  describe "Bar" $ do
    let a = Bar (NChains (WbarDiscrete (Zmod 3)))
    describe "is a bicomplex" $ do
      let as = do
            h <- [0 .. 5]
            v <- [0 .. 5]
            bibasis a (h, v)
      BicomplexProperties.checkChainConditions a as
    describe "is a chain complex" $
      ChainComplexProperties.checkChainConditionOn a "bar" ([0 .. 5] >>= CC.basis a)

  describe "BarBar" $ do
    let a = Bar (Bar (NChains (WbarDiscrete (Zmod 3))))
    describe "is a bicomplex" $ do
      let as = do
            h <- [0 .. 6]
            v <- [0 .. 6]
            bibasis a (h, v)
      BicomplexProperties.checkChainConditions a as
    describe "is a chain complex" $
      ChainComplexProperties.checkChainConditionOn a "bar" ([0 .. 6] >>= CC.basis a)
