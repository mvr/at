module Math.Topology.SSet.CircleSpec where

import Test.Hspec

import Math.Algebra.AbGroupPres (freeAbGroup)
import Math.Topology.SSet.Circle
import Math.Topology.SSet.Effective

import qualified Math.Topology.SSet.Properties as SSetProperties

spec :: Spec
spec = describe "circle" $ do
  describe "SSet" $
    SSetProperties.check 4 Circle

  it "has the homology of S^1" $
    take 4 (homology Circle)
      `shouldBe` [freeAbGroup 1, freeAbGroup 1, freeAbGroup 0, freeAbGroup 0]
