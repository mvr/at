-- import Data.Coerce

-- import Math.Topology.SSet.DVF

import qualified AbGroupPresTest
import qualified AbelianCategoryProperties
import Control.Monad (forM_)
import qualified DVFProperties
import Data.Proxy
import Math.Algebra.AbGroupPres
import qualified Math.Algebra.ChainComplex as CC
import Math.Algebra.Bicomplex hiding (FiniteType)
import Math.Algebra.ChainComplex.DVF
import Math.Algebra.ChainComplex.Algebra.Bar
import Math.Algebra.ChainComplex.Tensor
import Math.Algebra.Group
import Math.Topology.SGrp ()
import Math.Topology.SGrp.Wbar
import Math.Topology.SGrp.KZ1 as KZ1 ()
import Math.Topology.SGrp.KZmod2_1
import Math.Topology.SGrp.WbarDiscrete
import Math.Topology.SSet
import Math.Topology.SSet.NormalisedChains
import Math.Topology.SSet.Product as Product
import Math.Topology.SSet.Sphere
import qualified MatrixOpsTest
import qualified ReductionProperties
import qualified SSetProperties
import qualified SGrpProperties
import qualified BicomplexProperties
import qualified ChainComplexProperties
import qualified SmithNormalFormTest
import Test.Hspec

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
    ChainComplexProperties.checkIso
      n
      (CriticalComplex (NormalisedChains (Product a b)))
      (Tensor (NormalisedChains a) (NormalisedChains b))
      Product.criticalIso
      (Product.criticalIsoInv a b)

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

  describe "products" $
    describe "S³ × S²" $ testProduct 7 (Sphere 3) (Sphere 2)

  describe "efficient K(ℤ/2,1)" $ do
    let p = KZmod2_1
    describe "SSet" $
      SSetProperties.check 4 p
    describe "SGrp" $
      SGrpProperties.check 4 p

  describe "K(ℤ,1)" $ do
    let p = WbarDiscrete Z
        ns = [-3 .. -1] ++ [1 .. 3]
        gs = [0 .. 3] >>= (\d -> sequence (replicate d ns))

        criticalBasis = [[], [1]]
        -- circlebasis = [Left (), Right (ShiftBasis ())]

    describe "SSet" $
      SSetProperties.checkOn p gs

    describe "DVF" $
      DVFProperties.checkOn (NormalisedChains p)  (BasisSimplex <$> gs)

    describe "dvfReduction" $
      ReductionProperties.checkOn
        (NormalisedChains p)
        (CriticalComplex (NormalisedChains p))
        (BasisSimplex <$> gs)
        (CriticalBasis <$> BasisSimplex <$> criticalBasis)
        (dvfReduction (NormalisedChains p))

  describe "K(ℤ/n,1)s" $ do
    forM_ [2, 3, 4, 5] $ \i -> do
      describe ("K(ℤ/" ++ show i ++ ",1)") $ do
        let p = WbarDiscrete (Zmod i)
        describe "SSet" $
          SSetProperties.check 4 p
        describe "SGrp" $
          SGrpProperties.check 4 p
        describe "DVF" $
          DVFProperties.check 4 (NormalisedChains p)

  describe "K(ℤ/3,2)" $ do
    let p = Wbar (WbarDiscrete (Zmod 3))
    describe "SSet" $
      SSetProperties.check 4 p
    describe "DVF" $
      DVFProperties.check 4 (NormalisedChains p)

  describe "Bar" $ do
    let a = Bar (NormalisedChains (WbarDiscrete (Zmod 3)))
    describe "is a bicomplex" $ do
      let as = do
            h <- [0 .. 5]
            v <- [0 .. 5]
            bibasis a (h, v)
      BicomplexProperties.checkChainConditions a as
    describe "is a chain complex" $ do
      let as = [0 .. 5] >>= CC.basis a

      ChainComplexProperties.checkChainCondition a "bar" as
