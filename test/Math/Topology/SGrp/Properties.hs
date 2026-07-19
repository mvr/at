-- |
module Math.Topology.SGrp.Properties where

import Control.Monad (forM_)
import qualified Math.Topology.SSet.Properties as SSet
import Test.Hspec
import Prelude hiding (id, (.))

import Math.Algebra.Group
import Math.Topology.SGrp
import Math.Topology.SSet
import Math.Topology.SSet.Product

check :: (SGrp a, FiniteType a, Show (GeomSimplex a)) => Int -> a -> Spec
check n a = do
  describe "group operation should be an SSet morphism" $
    SSet.checkMorphismOn (Product a a) a (prodMor a) ([0 .. n] >>= geomBasis (Product a a))

  describe "inverse should be an SSet morphism" $
    SSet.checkMorphismOn a a (invMor a) ([0 .. n] >>= geomBasis a)

  it "simplices should satisfy the group laws" $
    forM_ [0 .. n] $ \d -> do
      let group = NDimSimplicesOf d a
          simplices =
            take 6 (NonDegen <$> geomBasis a d)
              ++ take 6 (filter isDegen (allSimplices a d))
          identity = unit group
      forM_ simplices $ \x -> do
        prod group identity x `shouldBe` x
        prod group x identity `shouldBe` x
        prod group (inv group x) x `shouldBe` identity
        prod group x (inv group x) `shouldBe` identity
      forM_ [(x, y, z) | x <- simplices, y <- simplices, z <- simplices] $ \(x, y, z) ->
        prod group (prod group x y) z `shouldBe` prod group x (prod group y z)
