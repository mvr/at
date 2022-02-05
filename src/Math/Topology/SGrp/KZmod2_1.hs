{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

-- | The space \(K(\mathbb{Z}/2, 1)\)
--
-- Ugly module name, but what can you do?
module Math.Topology.SGrp.KZmod2_1 where

import Math.Topology.SGrp
import Math.Topology.SSet
import Math.Topology.SSet.Effective
import Math.Topology.SSet.Morphism

data KZmod2_1 = KZmod2_1

instance SSet KZmod2_1 where
  type GeomSimplex KZmod2_1 = Int

  isGeomSimplex KZmod2_1 s = s >= 0

  geomSimplexDim _ s = s

  geomFace _ 0 _ = undefined
  geomFace _ s 0 = NonDegen (s - 1)
  geomFace _ s i | s == i = NonDegen (s - 1)
  geomFace _ s i = Degen (i - 1) (NonDegen (s - 2))

instance Pointed KZmod2_1 where
  basepoint _ = 0

instance ZeroReduced KZmod2_1

instance FiniteType KZmod2_1 where
  geomBasis _ i = [i]

instance Effective KZmod2_1

instance SGrp KZmod2_1 where
  prodMor _ = undefined -- TODO: unnormalise?
  invMor _ = Morphism NonDegen

instance SAb KZmod2_1
