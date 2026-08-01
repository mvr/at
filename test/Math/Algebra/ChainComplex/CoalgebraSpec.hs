module Math.Algebra.ChainComplex.CoalgebraSpec where

import Control.Category.Constrained ((.))
import Test.Hspec
import Prelude hiding ((.))

import qualified Math.Algebra.ChainComplex as CC
import Math.Algebra.ChainComplex.Coalgebra
import Math.Algebra.Combination (singleComb)
import Math.Topology.SSet.NChains
import Math.Topology.SSet.Product ()
import Math.Topology.SSet.Sphere

import qualified Math.Algebra.ChainComplex.Properties as ChainComplexProperties

spec :: Spec
spec = describe "coaugmented chains" $ do
  let chains = NChains (Sphere 2)
      eta = coaugmentationMor chains
      epsilon = counitMor chains

  describe "coaugmentation" $
    ChainComplexProperties.checkChainMap () chains "eta" [()] eta

  it "is split by the counit" $
    (epsilon . eta) `CC.onBasis` () `shouldBe` singleComb ()

  it "has zero reduced diagonal on the coaugmentation" $
    reducedDelMor chains `CC.onBasis` Basepoint `shouldBe` 0

  it "recognises the sphere cell as primitive" $
    reducedDelMor chains `CC.onBasis` Cell `shouldBe` 0
