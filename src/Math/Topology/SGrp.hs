-- | Simplicial Group

module Math.Topology.SGrp where

import Math.Topology.SSet
import Math.Topology.SSet.Morphism
import Math.Topology.SSet.Product

import Math.Algebra.Group

class (SSet a, Pointed a) => SGrp a where
  prod :: a -> Morphism (Product a a) a
  -- The identity map is always just picking out a 0-simplex, so we
  -- can assume a is pointed.
  inv :: a -> Morphism a a

isUnit :: (Pointed g, Eq (GeomSimplex g)) => g -> Simplex g -> Bool
isUnit g (NonDegen s) = s == basepoint g
isUnit g (Degen _ s) = isUnit g s

-- The set of n-simplices in a simplicial group forms a group.
data NSimplicesOf a = NSimplicesOf Int a

instance (SGrp a) => Group (NSimplicesOf a) where
  type Element (NSimplicesOf a) = Simplex a
  prod (NSimplicesOf n a) s t = Math.Topology.SGrp.prod a `mapSimplex` prodNormalise (Product a a) (s, t)
  unit (NSimplicesOf n a) = constantAtVertex a (basepoint a) n
  inv (NSimplicesOf n a) s = Math.Topology.SGrp.inv a `mapSimplex` s

class SGrp a => SAb a where

instance (SAb a) => Abelian (NSimplicesOf a)
