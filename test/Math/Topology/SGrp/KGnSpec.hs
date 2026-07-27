module Math.Topology.SGrp.KGnSpec where

import Test.Hspec

import Math.Algebra.ChainComplex
import Math.Algebra.ChainComplex.Algebra
import Math.Algebra.ChainComplex.Shift
import Math.Algebra.ChainComplex.Sum
import Math.Algebra.Combination
import Math.Algebra.Group
import Math.Topology.SGrp.KGn
import Math.Topology.SGrp.KGn.Cocycle
import Math.Topology.SGrp.KGn.DoldKan.Wbar (
  doldKanComparison,
  doldKanModel,
 )
import Math.Topology.SGrp.Wbar
import Math.Topology.SGrp.WbarDiscrete
import Math.Topology.SSet (geomBasis)

import qualified Math.Algebra.ChainComplex.Algebra.Properties as AlgebraProperties
import qualified Math.Topology.SGrp.Properties as SGrpProperties
import qualified Math.Topology.SSet.Properties as SSetProperties

circle :: CircleComplex
circle = Sum () (Shift ())

spec :: Spec
spec = do
  describe "circle chain algebra" $ do
    AlgebraProperties.check 3 circle

    it "squares the degree-one generator to zero" $
      muMor circle `onBasis` (Right (ShiftBasis ()), Right (ShiftBasis ()))
        `shouldBe` (0 :: Combination (Basis CircleComplex))

  describe "efficient K(ℤ/2,1)" $ do
    describe "SSet" $
      SSetProperties.check 4 KZmod2_1
    describe "SGrp" $
      SGrpProperties.check 4 KZmod2_1

  describe "Dold-Kan comparison" $ do
    let z3 = Zmod 3
        bar1 = WbarDiscrete z3
        standard1 = doldKanModel bar1
        bar2 = Wbar bar1
        standard2 = doldKanModel bar2
        bar3 = Wbar bar2
        standard3 = doldKanModel bar3
        efficient1 = doldKanModel KZmod2_1

    describe "in degree one" $
      SSetProperties.checkMorphismOn
        standard1
        bar1
        (doldKanComparison bar1)
        ([0 .. 3] >>= geomBasis standard1)

    describe "in degree two" $
      SSetProperties.checkMorphismOn
        standard2
        bar2
        (doldKanComparison bar2)
        ([0 .. 3] >>= geomBasis standard2)

    describe "in degree three" $
      SSetProperties.checkMorphismOn
        standard3
        bar3
        (doldKanComparison bar3)
        ([0 .. 4] >>= geomBasis standard3)

    describe "to the efficient K(ℤ/2,1) model" $
      SSetProperties.checkMorphismOn
        efficient1
        KZmod2_1
        (doldKanComparison KZmod2_1)
        ([0 .. 3] >>= geomBasis efficient1)

  describe "iterated Eilenberg-Mac Lane spaces" $
    it "iterates from the degree of the supplied space" $
      case iteratedEilenbergMacLane 3 (Wbar kz1) of
        SomeEilenbergMacLane g -> emDegree g `shouldBe` 3
