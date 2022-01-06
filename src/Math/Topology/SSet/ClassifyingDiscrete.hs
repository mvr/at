{-# LANGUAGE UndecidableInstances #-}

-- | Classifying spaces for discrete groups
-- Wbar : Grp -> 0-reduced sSet_*
-- Much easier than the case of general simplicial groups
-- See also https://dl.acm.org/doi/10.1145/1576702.1576744
module Math.Topology.SSet.ClassifyingDiscrete where

-- import Math.Topology.SSet.Effective

import Math.Algebra.Group
import qualified Math.Topology.SGrp as S
import Math.Topology.SSet

-- If a is a discrete group, things get much easier.
newtype ClassifyingDiscrete a = ClassifyingDiscrete a

normalise :: (Group a, Eq (Element a)) => a -> [Element a] -> Simplex (ClassifyingDiscrete a)
normalise a [] = NonDegen []
normalise a (e:es)
  | e == unit a = degen (ClassifyingDiscrete a) (normalise a es) 0
  | otherwise   = fmap (e:) (downshift (ClassifyingDiscrete a) (normalise a es))

instance (Group a, Eq (Element a)) => SSet (ClassifyingDiscrete a) where
  -- A non-degenerate n-simplex is a list of n non-identity elements
  -- of `a`
  type GeomSimplex (ClassifyingDiscrete a) = [Element a]

  isGeomSimplex (ClassifyingDiscrete a) ss = undefined -- not (any (isUnit a) ss)

  geomSimplexDim _ ss = length ss

  geomFace _ [] _ = undefined
  geomFace (ClassifyingDiscrete a) ss i = normalise a (underlying ss i)
    where underlying ss 0 = tail ss
          underlying [s] 1 = []
          underlying (s : s' : ss) 1 = prod a s s' : ss
          underlying (s : ss) i = s : underlying ss (i - 1)
          underlying _ _ = undefined -- can't happe

instance Group a => Pointed (ClassifyingDiscrete a) where
  basepoint (ClassifyingDiscrete a) = []

-- Eventually:
-- instance (SGrp g, Effective a) => Effective (ClassifyingDiscrete a)
--   type Model (ClassifyingDiscrete a) = Bar (Model a)

instance (Abelian a, Eq (Element a)) => S.SGrp (ClassifyingDiscrete a) where

instance (Abelian a, Eq (Element a)) => S.SAb (ClassifyingDiscrete a)
