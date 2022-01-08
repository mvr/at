-- | A simplicial morphism

module Math.Topology.SSet.Morphism where

import Control.Category.Constrained
import Math.Topology.SSet

newtype Morphism a b = Morphism { mapGeomSimplex :: GeomSimplex a -> Simplex b }

mapSimplex :: Morphism a b -> Simplex a -> Simplex b
mapSimplex (Morphism f) (NonDegen s) = f s
mapSimplex m (Degen i s) = Degen i (mapSimplex m s)

instance Category Morphism where
  id = Morphism $ \s -> NonDegen s
  f2 . (Morphism f1) = Morphism $ \s -> f2 `mapSimplex` f1 s
