module Math.Topology.SSet.RPnSpec where

import Control.Monad (forM_)
import Test.Hspec

import Math.Topology.SSet.RPn

import qualified Math.Topology.SSet.Properties as SSetProperties

spec :: Spec
spec = describe "real projective spaces" $
  forM_ [2, 3] $ \dimension ->
    describe ("RP^" ++ show dimension) $
      SSetProperties.check dimension (RPn dimension)
