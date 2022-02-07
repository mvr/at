{-# LANGUAGE UndecidableInstances #-}

-- | Classifying spaces for discrete groups
-- Wbar : Grp -> 0-reduced sSet_*
-- Much easier than the case of general simplicial groups
-- See also https://dl.acm.org/doi/10.1145/1576702.1576744
module Math.Topology.SGrp.WbarDiscrete where

import Math.Algebra.Group
import qualified Math.Topology.SGrp as S
import Math.Topology.SSet

-- If a is a discrete group, things get much easier.
newtype WbarDiscrete a = WbarDiscrete a

normalise :: (Group a, Eq (Element a)) => a -> [Element a] -> Simplex (WbarDiscrete a)
normalise a [] = NonDegen []
normalise a (e : es)
  | e == unit a = degen (normalise a es) 0
  | otherwise = fmap (e :) (downshift (normalise a es))

unnormalise :: (Group a, Eq (Element a)) => a -> Simplex (WbarDiscrete a) -> [Element a]
unnormalise a (NonDegen g) = g
unnormalise a (Degen i g) =
  let (before, after) = splitAt i (unnormalise a g)
  in before ++ [unit a] ++ after

instance (Group a, Eq (Element a)) => SSet (WbarDiscrete a) where
  -- A non-degenerate n-simplex is a list of n non-identity elements
  -- of `a`
  type GeomSimplex (WbarDiscrete a) = [Element a]

  isGeomSimplex (WbarDiscrete a) ss = unit a `notElem` ss

  geomSimplexDim _ ss = length ss

  geomFace _ [] _ = undefined
  geomFace (WbarDiscrete a) ss i = normalise a (underlying ss i)
    where
      underlying ss 0 = tail ss
      underlying [s] 1 = []
      underlying (s : s' : ss) 1 = prod a s s' : ss
      underlying (s : ss) i = s : underlying ss (i - 1)
      underlying _ _ = undefined -- can't happen

instance (Group a, Eq (Element a)) => Pointed (WbarDiscrete a) where
  basepoint (WbarDiscrete a) = []

instance (Group a, Eq (Element a)) => ZeroReduced (WbarDiscrete a)

instance (FiniteGroup a, Eq (Element a)) => FiniteType (WbarDiscrete a) where
  geomBasis (WbarDiscrete a) i = sequence (replicate i nonident)
    where nonident = filter (\x -> x /= unit a) (elements a)

instance (Abelian a, Eq (Element a)) => S.SGrp (WbarDiscrete a) where
  prodMor (WbarDiscrete a) = Morphism $ \(s, t) -> normalise a $ fmap (uncurry (prod a)) (zip (unnormalise a s) (unnormalise a t))
  invMor (WbarDiscrete a) = Morphism $ \s -> NonDegen $ fmap (inv a) s

instance (Abelian a, Eq (Element a)) => S.SAb (WbarDiscrete a)
