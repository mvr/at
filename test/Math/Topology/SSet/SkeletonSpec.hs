module Math.Topology.SSet.SkeletonSpec where

import Test.Hspec

import Math.Topology.SSet
import Math.Topology.SSet.NSimplex
import Math.Topology.SSet.Skeleton

import qualified Math.Topology.SSet.Properties as SSetProperties

spec :: Spec
spec = describe "simplicial skeletons" $ do
  let skeleton = Skeleton 2 (NSimplex 4)

  SSetProperties.check 4 skeleton

  it "has no basis simplices above the cutoff" $
    geomBasis skeleton 3 `shouldBe` []
