module Math.Topology.SSet.MooreSpec where

import Control.Monad (forM_)
import Test.Hspec

import Math.Topology.SSet.Moore

import qualified Math.Topology.SSet.Properties as SSetProperties

spec :: Spec
spec = describe "Moore spaces" $
  forM_ [2, 3] $ \order ->
    describe ("M(Z/" ++ show order ++ ", 5)") $
      SSetProperties.check 6 (Moore order 5)
