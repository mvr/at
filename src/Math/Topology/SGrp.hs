{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

-- | Simplicial Group
module Math.Topology.SGrp where

import Control.Category.Constrained
import qualified Math.Algebra.ChainComplex as CC (UMorphism (..))
import Math.Algebra.ChainComplex.Algebra
import Math.Algebra.ChainComplex.Reduction
import Math.Algebra.Group
import Math.Topology.SSet
import Math.Topology.SSet.NormalisedChains
import Math.Topology.SSet.Product
import Prelude hiding (return, (.), id)

class (SSet a, Pointed a) => SGrp a where
  -- The identity map is always just picking out a 0-simplex, so we
  -- can assume a is pointed.
  prodMor :: a -> Morphism (Product a a) a
  invMor :: a -> Morphism a a

class SGrp a => SAb a

isUnit :: Pointed g => g -> Simplex g -> Bool
isUnit g (NonDegen s) = s == basepoint g
isUnit g (Degen _ s) = isUnit g s

instance (SGrp a, SGrp b) => SGrp (Product a b) where
  -- TODO: this does more normalising/unnormalising than necessary,
  -- some rewrite rules might fix that.
  prodMor (Product a b) = (prodMor a × prodMor b) . αi . (id × α) . (id × (s × id)) . (id × αi) . α
    where (×) = prodFunc
          α = prodAssoc
          αi = prodAssocInv
          s = prodSym
  invMor (Product a b) = invMor a × invMor b
    where (×) = prodFunc

instance SGrp g => Algebra (NormalisedChains g) where
  unitMor (NormalisedChains g) = CC.Morphism 0 (const (return (BasisSimplex (basepoint g))))
  muMor (NormalisedChains g) = cfmap (prodMor g) . reductionG (ezReduction (Product g g))

-- The set of n-simplices in a simplicial group forms a group.
data NSimplicesOf a = NSimplicesOf Int a

instance (SGrp a) => Group (NSimplicesOf a) where
  type Element (NSimplicesOf a) = Simplex a
  prod (NSimplicesOf n a) s t = prodMor a `onSimplex` prodNormalise (s, t)
  unit (NSimplicesOf n a) = constantAt (basepoint a) n
  inv (NSimplicesOf n a) s = invMor a `onSimplex` s

instance (SAb a) => Abelian (NSimplicesOf a)
