module Math.Topology.SSet.SphereSpec where

import Control.Monad (forM_)
import Test.Hspec

import Math.Topology.SSet.Sphere

import qualified Math.Topology.SSet.Properties as SSetProperties

spec :: Spec
spec = describe "spheres" $
  forM_ [2, 3] $ \dimension ->
    describe ("S^" ++ show dimension) $
      SSetProperties.check dimension (Sphere dimension)
