module Math.Topology.SSet.WhiteheadSpec where

import Control.Monad (forM_)
import Test.Hspec

import Math.Algebra.AbGroupPres
import Math.Algebra.AbGroupPres.IsoClass
import qualified Math.Algebra.ChainComplex as CC
import Math.Algebra.Group
import Math.Topology.SGrp.KGn
import Math.Topology.SGrp.KGn.Cocycle
import Math.Topology.SGrp.Wbar
import Math.Topology.SGrp.WbarDiscrete
import Math.Topology.SSet
import Math.Topology.SSet.Effective
import qualified Math.Topology.SSet.Moore as Moore
import Math.Topology.SSet.NChains
import qualified Math.Topology.SSet.Product as Product
import Math.Topology.SSet.Sphere
import Math.Topology.SSet.TwistedProduct
import Math.Topology.SSet.Whitehead
import qualified Math.Topology.SSet.Properties as SSetProperties

onlyCocycle :: CC.FiniteType a => a -> Int -> CC.FundamentalCocycle a
onlyCocycle a degree = case CC.fundamentalCocycles a degree of
  Right [cocycle] -> cocycle
  _ -> error "expected exactly one fundamental cocycle"

spec :: Spec
spec = describe "Whitehead tower" $ do
  describe "generated twisting functions" $ do
    it "reconstructs the Hopf twist over S2" $ do
      let s2 = Sphere 2
          cocycle = onlyCocycle (NChains s2) 2
      twistOnGeom (whiteheadTwist s2 kz1 cocycle) Cell `shouldBe` NonDegen [1]

    it "reconstructs the existing twist over S3" $ do
      let s3 = Sphere 3
          kz2 = Wbar kz1
          cocycle = onlyCocycle (NChains s3) 3
          expected = NonDegen $ wbarSimplex kz1 [NonDegen [1], NonDegen []]
      twistOnGeom (whiteheadTwist s3 kz2 cocycle) Cell `shouldBe` expected

    let productOfSpheres = Product.Product (Sphere 2) (Sphere 2)
        productCocycle = onlyCocycleForOrder (model productOfSpheres) 2 Nothing
        productClassifyingMap = cocycleClassifyingMap productOfSpheres kz1 productCocycle
        productTwist = whiteheadTwist productOfSpheres kz1 productCocycle
        productSimplices = [0 .. 4] >>= geomBasis productOfSpheres
    describe "classifying map over S2 x S2" $
      SSetProperties.checkMorphismOn productOfSpheres (Wbar kz1) productClassifyingMap productSimplices
    describe "twist over S2 x S2" $
      SSetProperties.checkTwistOn productOfSpheres kz1 productTwist productSimplices

    let productOfS3s = Product.Product (Sphere 3) (Sphere 3)
        productS3Cocycle = onlyCocycleForOrder (model productOfS3s) 3 Nothing
        productS3ClassifyingMap = cocycleClassifyingMap productOfS3s (Wbar kz1) productS3Cocycle
        productS3Twist = whiteheadTwist productOfS3s (Wbar kz1) productS3Cocycle
        productS3Simplices = [0 .. 5] >>= geomBasis productOfS3s
    describe "classifying map over S3 x S3" $
      SSetProperties.checkMorphismOn productOfS3s (Wbar (Wbar kz1)) productS3ClassifyingMap productS3Simplices
    describe "twist over S3 x S3" $
      SSetProperties.checkTwistOn productOfS3s (Wbar kz1) productS3Twist productS3Simplices

    let moore5 = Moore.Moore 2 5
        moore5Cocycle = onlyCocycleForOrder (model moore5) 5 (Just 2)
        kzmod2_4 = Wbar (Wbar (Wbar KZmod2_1))
        moore5ClassifyingMap = cocycleClassifyingMap moore5 kzmod2_4 moore5Cocycle
        moore5Twist = whiteheadTwist moore5 kzmod2_4 moore5Cocycle
        moore5Simplices = [0 .. 6] >>= geomBasis moore5
    describe "classifying map over M(Z/2,5)" $
      SSetProperties.checkMorphismOn moore5 (Wbar kzmod2_4) moore5ClassifyingMap moore5Simplices
    describe "twist over M(Z/2,5)" $
      SSetProperties.checkTwistOn moore5 kzmod2_4 moore5Twist moore5Simplices

  describe "stage construction" $ do
    it "builds the integral stage over S2" $ do
      let s2 = Sphere 2
          cocycle = onlyCocycle (model s2) 2
          TwistedProduct _ _ _ _ twist = whiteheadStage s2 kz1 cocycle
      twistOnGeom twist Cell `shouldBe` NonDegen [1]
      forM_ [Basepoint, Cell] $ SSetProperties.checkTwistFaces s2 kz1 twist

    it "builds an odd-torsion stage over a Moore space" $ do
      let moore5 = Moore.Moore 3 5
          kzmod3_4 = Wbar (Wbar (Wbar (WbarDiscrete (Zmod 3))))
          cocycle = onlyCocycleForOrder (model moore5) 5 (Just 3)
          TwistedProduct _ _ _ _ twist = whiteheadStage moore5 kzmod3_4 cocycle
      twistOnGeom twist Moore.N
        `shouldNotBe` constantAt (basepoint kzmod3_4) 4
      forM_ [Moore.Basepoint, Moore.N, Moore.NPlusOne] $
        SSetProperties.checkTwistFaces moore5 kzmod3_4 twist

  describe "homotopy groups" $ do
    it "returns every group through the target degree in ascending order" $
      homotopyGroupsThrough 4 (Sphere 2)
        `shouldBe` Right
          [ (2, freeAbGroup 1),
            (3, freeAbGroup 1),
            (4, fromIsoClass $ IsoClass 0 [(2, 1)])
          ]

    it "retains zero groups before the first nonzero homotopy group" $
      homotopyGroupsThrough 4 (Sphere 4)
        `shouldBe` Right
          [ (2, freeAbGroup 0),
            (3, freeAbGroup 0),
            (4, freeAbGroup 1)
          ]

    it "rejects target degrees below two" $ do
      homotopyGroupsThrough 1 (Sphere 2)
        `shouldBe` Left (InvalidHomotopyDegree 1)
      homotopyGroup 0 (Sphere 2)
        `shouldBe` Left (InvalidHomotopyDegree 0)

    it "computes pi_3(S2) with one stage" $
      homotopyGroup 3 (Sphere 2) `shouldBe` Right (freeAbGroup 1)

    it "computes pi_4(S2) with two stages" $
      homotopyGroup 4 (Sphere 2) `shouldBe` Right (fromIsoClass $ IsoClass 0 [(2, 1)])

    it "computes pi_4(S3) with one stage" $
      homotopyGroup 4 (Sphere 3) `shouldBe` Right (fromIsoClass $ IsoClass 0 [(2, 1)])

    it "computes pi_5(S4) using K(Z,3)" $
      homotopyGroup 5 (Sphere 4) `shouldBe` Right (fromIsoClass $ IsoClass 0 [(2, 1)])

    it "computes pi_5(S3) using K(Z/2,3)" $
      homotopyGroup 5 (Sphere 3) `shouldBe` Right (fromIsoClass $ IsoClass 0 [(2, 1)])

    it "computes pi_6(S3)" $
      homotopyGroup 6 (Sphere 3)
        `shouldBe` Right (fromIsoClass $ IsoClass 0 [(2, 2), (3, 1)])

    it "kills both generators when computing pi_3(S2 x S2)" $
      homotopyGroup 3 (Product.Product (Sphere 2) (Sphere 2)) `shouldBe` Right (freeAbGroup 2)

onlyCocycleForOrder :: (CC.FiniteType a) => a -> Int -> Maybe Integer -> CC.FundamentalCocycle a
onlyCocycleForOrder a degree order = case CC.fundamentalCocycles a degree of
  Right cocycles -> case filter ((== order) . CC.cocycleOrder) cocycles of
    cocycle : _ -> cocycle
    [] -> error "expected a fundamental cocycle with the requested order"
  Left err -> error err
