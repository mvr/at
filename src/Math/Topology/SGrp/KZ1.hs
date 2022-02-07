{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

-- | The space \(K(\mathbb{Z}, 1)\), homotopy equivalent to the
-- circle. We cannot use the same method as Wbar to show that this
-- simplicial set is effective, because \(\mathbb{Z}\) (as a discrete
-- \(sGrp\)) is not 0-reduced.
--
-- A good reference for the DVF used here is
-- <https://doi.org/10.1007/s10208-013-9159-7>
module Math.Topology.SGrp.KZ1 where

import Control.Category.Constrained ((.))
import Data.Coerce
import Math.Algebra.ChainComplex as CC
import Math.Algebra.ChainComplex.DVF hiding (DVF, vf)
import Math.Algebra.ChainComplex.Equivalence
import Math.Algebra.ChainComplex.Reduction
import Math.Algebra.ChainComplex.Shift
import Math.Algebra.ChainComplex.Sum
import Math.Algebra.Group
import Math.Topology.SGrp.WbarDiscrete
import Math.Topology.SSet.DVF
import Math.Topology.SSet.Effective
import Math.Topology.SSet.NormalisedChains
import Prelude hiding ((.))

type KZ1 = WbarDiscrete Z

kZ1 :: KZ1
kZ1 = WbarDiscrete Z

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

criticalIso :: CC.Morphism (CriticalComplex (NormalisedChains KZ1)) CircleComplex
criticalIso = fmapBasis $
  coerce $ \case
    [] -> Left ()
    [1 :: Integer] -> Right (ShiftBasis ())
    _ -> error "impossible"

criticalIsoInv :: CC.Morphism CircleComplex (CriticalComplex (NormalisedChains KZ1))
criticalIsoInv = fmapBasis $
  coerce $ \case
    Left () -> []
    Right (ShiftBasis ()) -> [1 :: Integer]

instance Effective KZ1 where
  type Model KZ1 = CircleComplex

  eff _ =
    fromRedLeft
      (NormalisedChains (WbarDiscrete Z))
      (Sum () (Shift ()))
      (isoToReduction criticalIso criticalIsoInv . dvfReduction (NormalisedChains (WbarDiscrete Z)))

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
