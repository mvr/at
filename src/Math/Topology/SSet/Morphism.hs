-- | A simplicial morphism

module Math.Topology.SSet.Morphism where

import Control.Category.Constrained
import Math.Topology.SSet
import Prelude hiding (Functor)

newtype UMorphism a b = Morphism { onGeomSimplex :: a -> FormalDegen b }

type Morphism a b = UMorphism (GeomSimplex a) (GeomSimplex b)

onSimplex :: UMorphism a b -> FormalDegen a -> FormalDegen b
onSimplex (Morphism f) (NonDegen s) = f s
onSimplex m (Degen i s) = degen (onSimplex m s) i

instance Semigroupoid UMorphism where
  f2 . (Morphism f1) = Morphism $ \s -> f2 `onSimplex` f1 s

instance Category UMorphism where
  id = Morphism $ \s -> NonDegen s
