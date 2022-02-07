{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

-- | The space \(K(\mathbb{Z}/2, 1)\)
--
-- Ugly module name, but what can you do?
module Math.Topology.SGrp.KZmod2_1 where

import Math.Topology.SGrp
import Math.Topology.SSet
import Math.Topology.SSet.Effective

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

mergeSorted :: Ord a => [a] -> [a] -> [a]
mergeSorted is [] = is
mergeSorted [] js = js
mergeSorted (i : is) (j : js)
  | i > j = i : mergeSorted is (j : js)
  | otherwise = j : mergeSorted (i:is) js

complement :: Int -> [Int] -> [Int]
complement (-1) [] = []
complement n [] = n : complement (n - 1) []
complement n (i : is)
  | n == i = complement (n-1) is
  | otherwise = n : complement (n - 1) (i:is)

instance SGrp KZmod2_1 where
  prodMor _ = Morphism $ \(s, t) ->
    let m = mergeSorted (degenList s) (degenList t) in
      foldr (flip degen) (NonDegen $ length m) (complement (simplexDim KZmod2_1 s - 1) m)
  invMor _ = Morphism NonDegen

instance SAb KZmod2_1
