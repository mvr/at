{-# LANGUAGE UndecidableInstances #-}
-- sue me
{-# OPTIONS_GHC -Wno-orphans #-}

-- | Simplicial Group
module Math.Topology.SGrp where

import Math.Algebra.ChainComplex.Algebra
import Math.Algebra.Group
import Math.Topology.NormalisedChains
import Math.Topology.SSet
import Math.Topology.SSet.Morphism
import Math.Topology.SSet.Product

class (SSet a, Pointed a) => SGrp a where
  prodMor :: a -> Morphism (Product a a) a
  -- The identity map is always just picking out a 0-simplex, so we
  -- can assume a is pointed.

  invMor :: a -> Morphism a a

isUnit :: (Pointed g, Eq (GeomSimplex g)) => g -> Simplex g -> Bool
isUnit g (NonDegen s) = s == basepoint g
isUnit g (Degen _ s) = isUnit g s

-- The set of n-simplices in a simplicial group forms a group.
data NSimplicesOf a = NSimplicesOf Int a

instance (SGrp a) => Group (NSimplicesOf a) where
  type Element (NSimplicesOf a) = Simplex a
  prod (NSimplicesOf n a) s t = prodMor a `mapSimplex` prodNormalise (Product a a) (s, t)
  unit (NSimplicesOf n a) = constantAt (basepoint a) n
  inv (NSimplicesOf n a) s = invMor a `mapSimplex` s

class SGrp a => SAb a

instance (SAb a) => Abelian (NSimplicesOf a)

instance (SGrp g, (Eq (GeomSimplex g))) => Algebra (NormalisedChains g) where


-- TODO
