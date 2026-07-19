module Math.Algebra.ChainComplex.HomSpec where

import Test.Hspec

import qualified Math.Algebra.ChainComplex as CC
import Math.Algebra.ChainComplex.Hom
import Math.Topology.SSet.NChains
import Math.Topology.SSet.RPn

import qualified Math.Algebra.ChainComplex.Properties as ChainComplexProperties

spec :: Spec
spec = describe "hom complex with nontrivial boundaries" $ do
  let c = NChains (RPn 2)
      h = Hom c c
      bs = [0 .. 2] >>= CC.basis h
  it "has a nonzero differential" $
    fmap (CC.onBasis (CC.diff h)) bs `shouldSatisfy` any (/= 0)
  ChainComplexProperties.checkChainCondition h 2
