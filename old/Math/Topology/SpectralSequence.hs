module Math.Topology.SpectralSequence where

import Math.Algebra.AbGroup.Effective
import Math.ValueCategory

data SpectralSequence ob = SpectralSequence
  { entry :: Integer -> Integer -> Integer -> ob,
    differential :: Integer -> Integer -> Integer -> Morphism ob
  }
