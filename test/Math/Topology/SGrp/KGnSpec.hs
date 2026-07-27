module Math.Topology.SGrp.KGnSpec where

import Test.Hspec

import Math.Algebra.ChainComplex
import Math.Algebra.ChainComplex.Algebra
import Math.Algebra.ChainComplex.Shift
import Math.Algebra.ChainComplex.Sum
import Math.Algebra.Combination
import Math.Algebra.Group
import Math.Topology.SGrp.KGn
import Math.Topology.SGrp.KGn.DoldKan.Cocycle
import Math.Topology.SGrp.KGn.DoldKan.Wbar (
  doldKanComparison,
  doldKanModel,
 )
import Math.Topology.SGrp.Wbar
import Math.Topology.SGrp.WbarDiscrete
import Math.Topology.SSet (geomBasis)
import Math.Topology.SSet.NChains
import Math.Topology.SSet.Sphere

import qualified Math.Algebra.ChainComplex.Algebra.Properties as AlgebraProperties
import qualified Math.Topology.SGrp.Properties as SGrpProperties
import qualified Math.Topology.SSet.Properties as SSetProperties

circle :: CircleComplex
circle = Sum () (Shift ())

sphereCocycle :: Cocycle (NChains Sphere) Z
sphereCocycle = Cocycle $ Cochain 2 $ \simplex -> case simplex of
  BasisSimplex Cell -> 1
  _ -> 0

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

  describe "cocycle Dold-Kan map" $ do
    let sphere = Sphere 2

    it "retains the simplex degree when evaluating a cocycle" $ do
      evaluateCocycleFaces sphere Z sphereCocycle Cell
        `shouldBe` CocycleFaceValues 2 [([0, 1], 1)]
      evaluateCocycleFaces sphere Z sphereCocycle Basepoint
        `shouldBe` CocycleFaceValues 0 []

    it "commutes with faces" $
      SSetProperties.checkMorphismFaces
        sphere
        (doldKanModel (Wbar kz1))
        (cocycleDoldKanMap sphere Z sphereCocycle)
        Cell

    it "compares with the iterated Wbar model" $
      SSetProperties.checkMorphismFaces
        sphere
        (Wbar kz1)
        (cocycleClassifyingMap sphere kz1 sphereCocycle)
        Cell
