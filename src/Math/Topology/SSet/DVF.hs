{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

-- | Discrete Vector Field on a \(sSet\). Also called an 'acyclic
-- matching':
-- <http://nlab-pages.s3.us-east-2.amazonaws.com/nlab/show/discrete+Morse+theory>
module Math.Topology.SSet.DVF where

import Data.Coerce
import qualified Math.Algebra.ChainComplex.DVF as CC
import Math.Topology.SSet
import Math.Topology.SSet.NChains

class SSet a => DVF a where
  -- TODO: Name??
  vf :: a -> GeomSimplex a -> CC.Status (GeomSimplex a)

instance DVF a => CC.DVF (NChains a) where
  vf (NChains a) (BasisSimplex b) = coerce $ vf a b
