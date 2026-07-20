module Math.Topology.SGrp.WbarDiscreteSpec where

import Control.Monad (forM_)
import Test.Hspec

import Math.Algebra.ChainComplex.DVF
import Math.Algebra.Group
import Math.Topology.SGrp.KGn ()
import Math.Topology.SGrp.WbarDiscrete
import Math.Topology.SSet
import Math.Topology.SSet.NChains

import qualified Math.Algebra.ChainComplex.DVF.Properties as DVFProperties
import qualified Math.Algebra.ChainComplex.Reduction.Properties as ReductionProperties
import qualified Math.Topology.SGrp.Properties as SGrpProperties
import qualified Math.Topology.SSet.Properties as SSetProperties

spec :: Spec
spec = do
  describe "K(ℤ,1)" $ do
    let p = WbarDiscrete Z
        ns = [-3 .. -1] ++ [1 .. 3]
        gs = [0 .. 3] >>= (\d -> sequence (replicate d ns))
        criticalBasis = [[], [1]]

    describe "SSet" $
      SSetProperties.checkOn p gs
    describe "DVF" $
      DVFProperties.checkOn (NChains p) (BasisSimplex <$> gs)
    describe "dvfReduction" $
      ReductionProperties.checkOn
        (NChains p)
        (CriticalComplex (NChains p))
        (BasisSimplex <$> gs)
        (CriticalBasis <$> BasisSimplex <$> criticalBasis)
        (dvfReduction (NChains p))

  describe "K(ℤ/n,1)s" $
    forM_ [2, 3, 4, 5] $ \i ->
      describe ("K(ℤ/" ++ show i ++ ",1)") $ do
        let group = Zmod i
            p = WbarDiscrete group
        it "normalises and unnormalises invertibly" $
          forM_ [0 .. 4] $ \degree -> do
            forM_ (sequence (replicate degree (elements group))) $ \entries ->
              unnormalise group (normalise group entries) `shouldBe` entries
            forM_ (allSimplices p degree) $ \simplex ->
              normalise group (unnormalise group simplex) `shouldBe` simplex
        describe "SSet" $
          SSetProperties.check 4 p
        describe "SGrp" $
          SGrpProperties.check 4 p
        describe "DVF" $
          DVFProperties.check 4 (NChains p)
