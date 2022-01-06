{-# OPTIONS_GHC -Wno-orphans #-}
{-# LANGUAGE UndecidableInstances #-}
-- | Discrete Vector Field on a sSet
module Math.Topology.SSet.DVF where

import qualified Math.Algebra.ChainComplex.DVF as CC
import Math.Topology.SSet
import Math.Topology.NormalisedChains

-- Units of Z

class SSet a => DVF a where
  -- TODO: Name??
  vf :: a -> GeomSimplex a -> CC.Status (GeomSimplex a)

instance (DVF a, Eq (GeomSimplex a)) => CC.DVF (NormalisedChains a) where
  vf (NormalisedChains a) b = vf a b
