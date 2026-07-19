module Math.Topology.SSet.TwistedProductSpec where

import Test.Hspec

import Math.Algebra.Group
import Math.Topology.SGrp.KGn
import Math.Topology.SGrp.Wbar
import Math.Topology.SGrp.WbarDiscrete
import Math.Topology.SSet
import qualified Math.Topology.SSet.Product as Product
import Math.Topology.SSet.Sphere
import Math.Topology.SSet.TwistedProduct

import qualified Math.Topology.SSet.Properties as SSetProperties

spec :: Spec
spec = do
  describe "PrincipalFibration over S2" $ do
    let s2 = Sphere 2
        classifying :: Morphism Sphere (Wbar KZ1)
        classifying = Morphism go
          where
            go Cell = NonDegen $ WbarSimplex [NonDegen [1], NonDegen []]
            go Basepoint = NonDegen $ WbarSimplex []

        fibration :: Twist Sphere KZ1
        fibration = pullback (Wbar kz1) kz1 (canonicalTwist kz1) classifying
        x :: TotalSpace Sphere KZ1
        x = totalSpace s2 kz1 fibration

        n = 3
        ks = [-3 .. -1] ++ [1 .. 3]
        gs =
          [ TwistedProductSimplex (s, t)
          | s <- someSimplices kz1 n (\d -> if d <= 3 then sequence (replicate d ks) else []),
            t <- allSimplices s2 n,
            isGeomSimplex (Product.Product kz1 s2) (s, t)
          ]

    describe "classifying morphism" $
      SSetProperties.checkMorphismOn s2 (Wbar kz1) classifying [Basepoint, Cell]
    describe "twist" $
      SSetProperties.checkTwistOn s2 kz1 fibration [Basepoint, Cell]
    describe "SSet" $
      SSetProperties.checkOn x gs

  describe "PrincipalFibration over S3" $ do
    let s3 = Sphere 3
        kz2 = Wbar kz1
        kz3 = Wbar kz2
        classifying = Morphism go
          where
            go Cell =
              NonDegen $
                WbarSimplex
                  [ NonDegen (WbarSimplex [NonDegen [1], NonDegen []]),
                    Degen 0 (NonDegen (WbarSimplex [])),
                    NonDegen (WbarSimplex [])
                  ]
            go Basepoint = NonDegen $ WbarSimplex []

        fibration = pullback kz3 kz2 (canonicalTwist kz2) classifying
        x = totalSpace s3 kz2 fibration

        n = 3
        ks = [-3 .. -1] ++ [1 .. 3]
        someKz1 d = someSimplices kz1 d (\i -> if i <= 3 then sequence (replicate i ks) else [])
        someGeomKz2 dimension =
          filter (isGeomSimplex kz2) $
            fmap WbarSimplex $
              sequence $
                someKz1 <$> reverse [0 .. dimension - 1]
        someKz2 dimension = someSimplices kz2 dimension someGeomKz2
        gs =
          [ TwistedProductSimplex (s, t)
          | s <- someKz2 n,
            t <- allSimplices s3 n,
            isGeomSimplex (Product.Product kz2 s3) (s, t)
          ]

    describe "classifying morphism" $
      SSetProperties.checkMorphismOn s3 kz3 classifying [Basepoint, Cell]
    describe "twist" $
      SSetProperties.checkTwistOn s3 kz2 fibration [Basepoint, Cell]
    describe "SSet" $
      SSetProperties.checkOn x gs

  describe "Universal principal fibration over K(ℤ/2,2)" $ do
    let g = WbarDiscrete (Zmod 2)
        b = Wbar g
        twist = canonicalTwist g
        x = totalSpace b g twist

    describe "twist" $
      SSetProperties.checkTwistOn b g twist ([0 .. 4] >>= geomBasis b)
    describe "SSet" $
      SSetProperties.check 4 x
