module Math.Topology.SSet.ProductSpec where

import Test.Hspec

import Math.Algebra.ChainComplex.DVF
import Math.Algebra.ChainComplex.Tensor
import Math.Topology.SSet
import Math.Topology.SSet.NChains
import Math.Topology.SSet.NSimplex
import qualified Math.Topology.SSet.Product as Product
import Math.Topology.SSet.Sphere

import qualified Math.Algebra.ChainComplex.DVF.Properties as DVFProperties
import qualified Math.Algebra.ChainComplex.Properties as ChainComplexProperties
import qualified Math.Algebra.ChainComplex.Reduction.Properties as ReductionProperties
import qualified Math.Topology.SSet.Properties as SSetProperties

checkProduct ::
  (FiniteType a, FiniteType b, Show (GeomSimplex a), Show (GeomSimplex b)) =>
  Int ->
  a ->
  b ->
  Spec
checkProduct n a b = do
  let p = Product.Product a b

  describe "SSet" $
    SSetProperties.check n p
  describe "DVF" $
    DVFProperties.check n (NChains p)

  it "criticalIso is a bijection" $
    ChainComplexProperties.checkIso
      n
      (CriticalComplex (NChains p))
      (Tensor (NChains a) (NChains b))
      Product.criticalIso
      (Product.criticalIsoInv a b)

  describe "dvfReduction" $
    ReductionProperties.check n (NChains p) (CriticalComplex (NChains p)) (dvfReduction (NChains p))
  describe "ezReduction" $
    ReductionProperties.check n (NChains p) (Tensor (NChains a) (NChains b)) (Product.ezReduction p)

recursiveProdNormalise :: (Simplex a, Simplex b) -> Simplex (Product.Product a b)
recursiveProdNormalise (Degen i s, Degen j t)
  | i == j = degen (recursiveProdNormalise (s, t)) i
  | i > j =
      let p = recursiveProdNormalise (s, Degen j t)
       in fmap (\(s', t') -> (Degen (i - degenCount p) s', t')) p
  | otherwise =
      let p = recursiveProdNormalise (Degen i s, t)
       in fmap (\(s', t') -> (s', Degen (j - degenCount p) t')) p
recursiveProdNormalise simplices = NonDegen simplices

spec :: Spec
spec = describe "products" $ do
  it "normalises degeneracy masks like the recursive algorithm" $ do
    let mismatches =
          [ (leftMask, rightMask)
            | leftMask <- [0 .. 255],
              rightMask <- [0 .. 255],
              let left = FormalDegen leftMask Cell :: Simplex Sphere
                  right = FormalDegen rightMask Cell :: Simplex Sphere,
              Product.prodNormalise (left, right) /= recursiveProdNormalise (left, right)
          ]
    mismatches `shouldBe` []

  it "recognises jointly nondegenerate mask pairs directly" $ do
    let mismatches =
          [ (leftMask, rightMask)
            | leftMask <- [0 .. 255],
              rightMask <- [0 .. 255],
              let left = FormalDegen leftMask Cell :: Simplex Sphere
                  right = FormalDegen rightMask Cell :: Simplex Sphere,
              Product.jointlyNonDegen (left, right)
                /= not (isDegen (recursiveProdNormalise (left, right)))
          ]
    mismatches `shouldBe` []

  describe "S³ × S²" $ checkProduct 7 (Sphere 3) (Sphere 2)
  describe "Δ² × Δ²" $ checkProduct 4 (NSimplex 2) (NSimplex 2)
