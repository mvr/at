{-# OPTIONS_GHC -Wno-orphans #-}
{-# LANGUAGE UndecidableInstances #-}
-- | Discrete Vector Field on a sSet
module Math.Topology.SSet.DVF where

import Data.Coerce
import qualified Math.Algebra.ChainComplex.DVF as CC
import Math.Topology.SSet
import Math.Topology.NormalisedChains

-- Units of Z

class SSet a => DVF a where
  -- TODO: Name??
  vf :: a -> GeomSimplex a -> CC.Status (GeomSimplex a)

instance DVF a => CC.DVF (NormalisedChains a) where
  vf (NormalisedChains a) (BasisSimplex b) = coerce $ vf a b
