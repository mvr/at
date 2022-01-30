import Test.Hspec
import Data.Proxy
-- import Data.Coerce

import Math.Algebra.AbGroupPres
import Math.Algebra.ChainComplex.Tensor

import Math.Topology.SSet
import Math.Topology.SSet.NormalisedChains
import Math.Topology.SSet.Sphere
import Math.Topology.SSet.Product
-- import Math.Topology.SSet.DVF
import Math.Algebra.ChainComplex.DVF

import qualified SmithNormalFormTest
import qualified MatrixOpsTest
import qualified AbGroupPresTest
import qualified AbelianCategoryProperties
import qualified ReductionProperties
import qualified SSetProperties
import qualified DVFProperties


main :: IO ()
main = hspec spec

testProduct :: (FiniteType a, FiniteType b, Show (GeomSimplex a), Show (GeomSimplex b)) => Int -> a -> b -> Spec
testProduct n a b = do
  let p = Product a b

  describe "SSet" $
    SSetProperties.check n p
  describe "DVF" $
    DVFProperties.check n (NormalisedChains p)

  it "criticalIso is a bijection" $
    ReductionProperties.checkIso
      n
      (CriticalComplex (NormalisedChains (Product a b)))
      (Tensor (NormalisedChains a) (NormalisedChains b))
      criticalIso
      (criticalIsoInv a b)

  describe "dvfReduction" $
    ReductionProperties.check n (NormalisedChains p) (CriticalComplex (NormalisedChains p)) (dvfReduction (NormalisedChains p))
  describe "ezReduction" $
    ReductionProperties.check n (NormalisedChains p) (Tensor (NormalisedChains a) (NormalisedChains b)) (ezReduction p)

spec :: Spec
spec = do
  SmithNormalFormTest.spec
  MatrixOpsTest.spec

  describe "Abelian category problems for AbGroup" $ AbelianCategoryProperties.spec (Proxy @AbGroupPres)
  -- describe "Abelian category problems for Cached AbGroup" $ AbelianCategoryProperties.spec (Proxy @AbGroupPres)
  AbGroupPresTest.spec

  describe "S³ × S²" $
    testProduct 6 (Sphere 3) (Sphere 2)
