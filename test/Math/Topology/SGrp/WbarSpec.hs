module Math.Topology.SGrp.WbarSpec where

import Control.Monad (forM_)
import Test.Hspec

import Math.Algebra.ChainComplex.Algebra.Bar
import Math.Algebra.Group
import Math.Topology.SGrp.KGn
import Math.Topology.SGrp.Wbar
import qualified Math.Topology.SGrp.Wbar as Wbar
import Math.Topology.SGrp.WbarDiscrete
import Math.Topology.SSet
import Math.Topology.SSet.Effective
import Math.Topology.SSet.NChains

import qualified Math.Algebra.ChainComplex.DVF.Properties as DVFProperties
import qualified Math.Algebra.ChainComplex.Properties as ChainComplexProperties
import qualified Math.Algebra.ChainComplex.Reduction.Properties as ReductionProperties
import qualified Math.Topology.SSet.Properties as SSetProperties

spec :: Spec
spec = do
  describe "K(ℤ/3,2)" $ do
    let g = WbarDiscrete (Zmod 3)
        p = Wbar g
    it "normalisation should be invertible" $
      forM_ [0 .. 5] $ \i ->
        forM_ (allSimplices p i) $ \s ->
          Wbar.normalise g (Wbar.unnormalise g s) `shouldBe` s
    it "does not report a false outer degeneracy" $ do
      let ss = [Degen 1 (NonDegen [1, 1]), Degen 1 (NonDegen [1]), Degen 0 (NonDegen []), NonDegen []]
      Wbar.normalise g ss `shouldBe` NonDegen (WbarSimplex ss)
    describe "SSet" $
      SSetProperties.check 4 p
    describe "DVF" $
      DVFProperties.check 4 (NChains p)

  describe "effective K(Z/2,3) model" $
    ChainComplexProperties.checkChainCondition (model (Wbar (Wbar KZmod2_1))) 8

  describe "lifted Wbar reduction for K(Z/2,1)" $
    ReductionProperties.check
      6
      (TensorSusp (NChains (Wbar KZmod2_1)))
      (TensorSusp (Bar (NChains KZmod2_1)))
      ( tensorAlgReduction
          (NChains (Wbar KZmod2_1))
          (Bar (NChains KZmod2_1))
          (wbarReduction (Wbar KZmod2_1))
      )

  describe "Wbar reduction for K(Z/2,1)" $
    ReductionProperties.check
      4
      (NChains (Wbar KZmod2_1))
      (Bar (NChains KZmod2_1))
      (wbarReduction (Wbar KZmod2_1))

  describe "Wbar reduction for K(Z/3,1)" $ do
    let g = WbarDiscrete (Zmod 3)
    ReductionProperties.check
      4
      (NChains (Wbar g))
      (Bar (NChains g))
      (wbarReduction (Wbar g))
