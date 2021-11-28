module Math.Topology.SpectralSequence where

import Math.ValueCategory
import Math.Algebra.AbGroup.Effective

data SpectralSequence ob = SpectralSequence {
  entry        :: Integer -> Integer -> Integer -> ob,
  differential :: Integer -> Integer -> Integer -> Morphism ob
}
