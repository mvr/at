import Control.Monad (forM_)
import Test.Hspec

-- import Data.Coerce
import Data.Proxy
import Debug.Trace

import Math.Algebra.AbGroupPres
import Math.Algebra.Bicomplex hiding (FiniteType)
import qualified Math.Algebra.ChainComplex as CC
import Math.Algebra.ChainComplex.Algebra.Bar
import Math.Algebra.ChainComplex.DVF
import Math.Algebra.ChainComplex.Hom
import Math.Algebra.ChainComplex.Tensor
import Math.Algebra.Group
import Math.Topology.SGrp ()
import Math.Topology.SGrp.KGn
import Math.Topology.SGrp.Wbar as Wbar
import Math.Topology.SGrp.WbarDiscrete
import Math.Topology.SSet
import Math.Topology.SSet.NChains
import Math.Topology.SSet.Product as Product
import Math.Topology.SSet.Sphere

import qualified AbGroupPresTest
import qualified AbelianCategoryProperties
import qualified BicomplexProperties
import qualified ChainComplexProperties
import qualified DVFProperties
import qualified MatrixOpsTest
import qualified ReductionProperties
import qualified SGrpProperties
import qualified SSetProperties
import qualified SmithNormalFormTest

main :: IO ()
main = hspec spec

testProduct :: (FiniteType a, FiniteType b, Show (GeomSimplex a), Show (GeomSimplex b)) => Int -> a -> b -> Spec
testProduct n a b = do
  let p = Product a b

  describe "SSet" $
    SSetProperties.check n p
  describe "DVF" $
    DVFProperties.check n (NChains p)

  it "criticalIso is a bijection" $
    ChainComplexProperties.checkIso
      n
      (CriticalComplex (NChains (Product a b)))
      (Tensor (NChains a) (NChains b))
      Product.criticalIso
      (Product.criticalIsoInv a b)

  describe "dvfReduction" $
    ReductionProperties.check n (NChains p) (CriticalComplex (NChains p)) (dvfReduction (NChains p))
  describe "ezReduction" $
    ReductionProperties.check n (NChains p) (Tensor (NChains a) (NChains b)) (ezReduction p)

spec :: Spec
spec = do
  SmithNormalFormTest.spec
  MatrixOpsTest.spec

  describe "Abelian category problems for AbGroup" $ AbelianCategoryProperties.spec (Proxy @AbGroupPres)
  -- describe "Abelian category problems for Cached AbGroup" $ AbelianCategoryProperties.spec (Proxy @AbGroupPres)
  AbGroupPresTest.spec

  describe "hom complex" $
    let c = NChains (Sphere 3)
        d = NChains (Sphere 2)
     in ChainComplexProperties.checkChainCondition (Hom c d) 10

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
      DVFProperties.checkOn (NChains p) (BasisSimplex <$> gs)

    describe "dvfReduction" $
      ReductionProperties.checkOn
        (NChains p)
        (CriticalComplex (NChains p))
        (BasisSimplex <$> gs)
        (CriticalBasis <$> BasisSimplex <$> criticalBasis)
        (dvfReduction (NChains p))

  describe "K(ℤ/n,1)s" $ do
    forM_ [2, 3, 4, 5] $ \i -> do
      describe ("K(ℤ/" ++ show i ++ ",1)") $ do
        let p = WbarDiscrete (Zmod i)
        describe "SSet" $
          SSetProperties.check 4 p
        describe "SGrp" $
          SGrpProperties.check 4 p
        describe "DVF" $
          DVFProperties.check 4 (NChains p)

  describe "K(ℤ/3,2)" $ do
    let g = WbarDiscrete (Zmod 3)
    let p = Wbar g
    it "normalisation should be invertible" $
      forM_ [0 .. 5] (\i -> forM_ (allSimplices p i) (\s -> Wbar.normalise g (Wbar.unnormalise g s) `shouldBe` s))
    describe "SSet" $
      SSetProperties.check 4 p
    describe "DVF" $
      DVFProperties.check 4 (NChains p) -- This is actually not enough to uncover errors

  -- TODO: normalise wrong on normalise (WbarDiscrete (Zmod 3)) [Degen 1 (NonDegen [1,1]), Degen 1 (NonDegen [1]), Degen 0 (NonDegen []), NonDegen []]

  describe "Bar" $ do
    let a = Bar (NChains (WbarDiscrete (Zmod 3)))
    describe "is a bicomplex" $ do
      let as = do
            h <- [0 .. 5]
            v <- [0 .. 5]
            bibasis a (h, v)
      BicomplexProperties.checkChainConditions a as
    describe "is a chain complex" $ do
      let as = [0 .. 5] >>= CC.basis a

      ChainComplexProperties.checkChainConditionOn a "bar" as
