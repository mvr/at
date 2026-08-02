module Math.Topology.SGrp.Properties (
  check,
  checkOn,
) where

import Control.Monad (forM_)
import qualified Math.Topology.SSet.Properties as SSet
import Test.Hspec
import Prelude hiding (id, (.))

import Math.Algebra.Group
import Math.Topology.SGrp
import Math.Topology.SSet
import Math.Topology.SSet.Product

check :: (SGrp a, FiniteType a, Show (GeomSimplex a)) => Int -> a -> Spec
check n a =
  checkWithSamples
    n
    a
    (geomBasis a)
    (allSimplices a)
    groupLawSamples
  where
    groupLawSamples degree =
      take 6 (NonDegen <$> geomBasis a degree)
        ++ take 6 (filter isDegen (allSimplices a degree))

checkOn ::
  (SGrp a, Show (GeomSimplex a)) =>
  Int ->
  a ->
  (Int -> [GeomSimplex a]) ->
  Spec
checkOn n a sampleBasis =
  checkWithSamples
    n
    a
    sampleBasis
    (\degree -> take 8 (sampledSimplices degree))
    (\degree -> take 6 (sampledSimplices degree))
  where
    sampledSimplices degree = someSimplices a degree sampleBasis

checkWithSamples ::
  (SGrp a, Show (GeomSimplex a)) =>
  Int ->
  a ->
  (Int -> [GeomSimplex a]) ->
  (Int -> [Simplex a]) ->
  (Int -> [Simplex a]) ->
  Spec
checkWithSamples n a basis productFactors simplicesInDegree = do
  describe "group operation should be an SSet morphism" $
    SSet.checkMorphismOn
      (Product a a)
      a
      (prodMor a)
      ([0 .. n] >>= productBasis)

  describe "inverse should be an SSet morphism" $
    SSet.checkMorphismOn a a (invMor a) ([0 .. n] >>= basis)

  it "simplices should satisfy the group laws" $
    forM_ [0 .. n] $ \degree -> do
      let group = NDimSimplicesOf degree a
          simplices = simplicesInDegree degree
          identity = unit group
      forM_ simplices $ \x -> do
        prod group identity x `shouldBe` x
        prod group x identity `shouldBe` x
        prod group (inv group x) x `shouldBe` identity
        prod group x (inv group x) `shouldBe` identity
      forM_
        [(x, y, z) | x <- simplices, y <- simplices, z <- simplices]
        $ \(x, y, z) ->
          prod group (prod group x y) z
            `shouldBe` prod group x (prod group y z)
  where
    productBasis degree =
      [ productSimplex
      | left <- factors,
        right <- factors,
        NonDegen productSimplex <- [prodNormalise (left, right)]
      ]
      where
        factors = productFactors degree
