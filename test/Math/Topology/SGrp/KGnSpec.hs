module Math.Topology.SGrp.KGnSpec where

import Test.Hspec

import Math.Algebra.ChainComplex
import Math.Algebra.ChainComplex.Algebra
import Math.Algebra.ChainComplex.Shift
import Math.Algebra.ChainComplex.Sum
import Math.Algebra.Combination
import Math.Topology.SGrp.KGn

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
