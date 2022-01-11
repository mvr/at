-- | A simplicial morphism

module Math.Topology.SSet.Morphism where

import Control.Category.Constrained
import Math.Topology.SSet

newtype UMorphism a b = Morphism { mapGeomSimplex :: a -> FormalDegen b }

type Morphism a b = UMorphism (GeomSimplex a) (GeomSimplex b)

mapSimplex :: UMorphism a b -> FormalDegen a -> FormalDegen b
mapSimplex (Morphism f) (NonDegen s) = f s
mapSimplex m (Degen i s) = Degen i (mapSimplex m s)

instance Category UMorphism where
  id = Morphism $ \s -> NonDegen s
  f2 . (Morphism f1) = Morphism $ \s -> f2 `mapSimplex` f1 s
