{-# LANGUAGE UndecidableInstances #-}

-- | A principle g-bundle over a, represented as a degree (-1) map of
-- simplices a -> g
module Math.Topology.SSet.PrincipalFibration where

import Math.Algebra.ChainComplex.DVF
import Math.Algebra.Group
import Math.Topology.NormalisedChains
import Math.Topology.SGrp
import Math.Topology.SSet
import Math.Topology.SSet.Product

-- twistOp has 'degree -1'
-- The following equations are satisfied:
-- ∂_i(τ b) = τ(∂_i b) if i < n-1
-- ∂_(n−1) (τ b) = [τ (∂_n b)]^(−1) . τ (∂_(n−1) b)
-- η_i(τ b) = τ(η_i b) if i <= n - 1
-- e_n = τ(η_n b)
newtype PrincipalFibration a g = PrincipalFibration {geomTwistOp :: GeomSimplex a -> Simplex g}

twistOp :: PrincipalFibration a g -> g -> Simplex a -> Simplex g
twistOp f g (NonDegen s) = geomTwistOp f s
twistOp f g (Degen i s) = degen (twistOp f g s) i

data TotalSpace a g = TotalSpace a g (PrincipalFibration a g)

instance (SSet a, SGrp g) => SSet (TotalSpace a g) where
  type GeomSimplex (TotalSpace a g) = (Simplex a, Simplex g)

  isGeomSimplex = undefined

  geomSimplexDim (TotalSpace a _ _) (s, _) = simplexDim a s

  geomFace (TotalSpace a g tau) (s, t) i
    | i == n =
      prodNormalise
        (Product a g)
        ( face a s n,
          prod
            (NSimplicesOf (n -1) g)
            (twistOp tau g s)
            (face g t n)
        )
    | otherwise = prodNormalise (Product a g) (face a s i, face g t i)
    where
      n = simplexDim a s

instance (SSet a, SGrp g, Eq (GeomSimplex a), Eq (GeomSimplex g)) => DVF (NormalisedChains (TotalSpace a g)) where
  vf = undefined
