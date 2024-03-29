-- |
module SGrpProperties where

import qualified SSetProperties as SSet
import Test.Hspec
import Prelude hiding (id, (.))

import Math.Topology.SGrp
import Math.Topology.SSet
import Math.Topology.SSet.Product

check :: (SGrp a, FiniteType a, Show (GeomSimplex a)) => Int -> a -> Spec
check n a = do
  describe "group operation should be SSet morphism" $
    SSet.checkMorphismOn (Product a a) a (prodMor a) (geomBasis (Product a a) n)

-- it "group operation should be a SSet morphism" $
--   forM_ [0 .. n] (\i -> forM_ (geomBasis a i) (\g -> checkMorphismOn ? ))
