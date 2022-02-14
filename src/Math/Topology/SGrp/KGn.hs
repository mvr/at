{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

-- | The space \(K(\mathbb{Z}, 1)\), homotopy equivalent to the
-- circle. We cannot use the same method as Wbar to show that this
-- simplicial set is effective, because \(\mathbb{Z}\) (as a discrete
-- \(sGrp\)) is not 0-reduced.
--
-- A good reference for the DVF used here is
-- <https://doi.org/10.1007/s10208-013-9159-7>
module Math.Topology.SGrp.KGn where

import Control.Category.Constrained ((.))
import Data.Coerce
import Math.Algebra.ChainComplex as CC hiding (FiniteType, Morphism)
import qualified Math.Algebra.ChainComplex as CC
import Math.Algebra.ChainComplex.DVF hiding (DVF, vf)
import Math.Algebra.ChainComplex.Equivalence
import Math.Algebra.ChainComplex.Reduction
import Math.Algebra.ChainComplex.Shift
import Math.Algebra.ChainComplex.Sum
import Math.Algebra.Group
import Math.Topology.SGrp
import Math.Topology.SGrp.WbarDiscrete
import Math.Topology.SSet
import Math.Topology.SSet.DVF
import Math.Topology.SSet.Effective
import Math.Topology.SSet.NChains
import Prelude hiding ((.))

type KZ1 = WbarDiscrete Z

type CircleComplex = () `Sum` Shift ()

instance DVF KZ1 where
  vf _ [] = Critical
  vf _ [1] = Critical
  vf _ (1 : a1 : as)
    | a1 < 0 = Target (a1 : as) Pos
    | otherwise = Target (a1 + 1 : as) Neg
  vf _ (a1 : as)
    | a1 < 0 = Source (1 : a1 : as) Pos
    | otherwise = Source (1 : a1 - 1 : as) Neg

criticalIso :: CC.Morphism (CriticalComplex (NChains KZ1)) CircleComplex
criticalIso = fmapBasis $
  coerce $ \case
    [] -> Left ()
    [1 :: Integer] -> Right (ShiftBasis ())
    _ -> error "impossible"

criticalIsoInv :: CC.Morphism CircleComplex (CriticalComplex (NChains KZ1))
criticalIsoInv = fmapBasis $
  coerce $ \case
    Left () -> []
    Right (ShiftBasis ()) -> [1 :: Integer]

instance Effective KZ1 where
  type Model KZ1 = CircleComplex

  eff _ =
    fromRedLeft
      (NChains (WbarDiscrete Z))
      (Sum () (Shift ()))
      (isoToReduction criticalIso criticalIsoInv . dvfReduction (NChains (WbarDiscrete Z)))

-- This seems to work... TODO: give a reduction from this to a chain
-- complex with one basis element in each dimension, no need to
-- generate all simplices and filter
instance DVF (WbarDiscrete Zmod) where
  vf (WbarDiscrete (Zmod n)) [] = Critical
  vf (WbarDiscrete (Zmod n)) [1] = Critical
  vf (WbarDiscrete (Zmod n)) (1 : a1 : as)
    | a1 == n - 1 = fmap (\x -> 1 : a1 : x) (vf (WbarDiscrete (Zmod n)) as)
    | otherwise = Target (a1 + 1 : as) Neg
  vf (WbarDiscrete (Zmod n)) (a1 : as) = Source (1 : a1 - 1 : as) Neg

-- | An efficient version of \(K(\mathbb{Z}/2, 1)\)
-- Ugly name, but what can you do?
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
  | otherwise = j : mergeSorted (i : is) js

complement :: Int -> [Int] -> [Int]
complement (-1) [] = []
complement n [] = n : complement (n - 1) []
complement n (i : is)
  | n == i = complement (n -1) is
  | otherwise = n : complement (n - 1) (i : is)

instance SGrp KZmod2_1 where
  prodMor _ = Morphism $ \(s, t) ->
    let m = mergeSorted (degenList s) (degenList t)
     in foldr (flip degen) (NonDegen $ length m) (complement (simplexDim KZmod2_1 s - 1) m)
  invMor _ = Morphism NonDegen

instance SAb KZmod2_1
