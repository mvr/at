{-# LANGUAGE UndecidableInstances #-}

-- | A principle \(G\)-bundle over \(A\), represented as a degree (-1) map of
-- simplicial sets \(τ : A \to G\).
--
-- The following equations are satisfied:
-- \[
-- \begin{aligned}
--   ∂_i(τ b) &= τ(∂_i b) && \text{if } i < n-1 \\
--   ∂_{n−1} (τ b) &= [τ (∂_n b)]^{−1} · τ (∂_{n−1} b) \\
--   η_i(τ_b) &= τ(η_i b) && \text{if } i ≤ n - 1 \\
--   e_n &= τ(η_n b)
-- \end{aligned}
-- \]
--
-- A twisting operation can be turned into a twisting cochain of the
-- corresponding normalised chain complexes, but this process is not
-- so simple. There are a few ways of obtaining it: a direct
-- complicated method by Szczarba, a recursive definition by Morace
-- and Prouté, and what Kenzo does. That is:
--
-- 1. Start with the Eilenberg-Zilber reduction \(A × G ⇛ A ⊗ G \)
-- 2. Determine the perturbation on \(A × G\) that yields \(A ×_τ G\)
-- 3. Use the perturbation lemma to transfer this down to \(A ⊗_t G\)
-- 4. Extract the twisting cochain \(t\) from \(A ⊗_t G\)
module Math.Topology.SSet.PrincipalFibration where

import Data.Coerce
import Math.Algebra.ChainComplex
import qualified Math.Algebra.ChainComplex as CC
import Math.Algebra.ChainComplex.Equivalence
import Math.Algebra.ChainComplex.Reduction
import Math.Algebra.ChainComplex.Tensor
import Math.Algebra.Group
import Math.Topology.NormalisedChains
import Math.Topology.SGrp
import Math.Topology.SSet
import Math.Topology.SSet.DVF
import Math.Topology.SSet.Effective
import Math.Topology.SSet.Product
import Prelude hiding (id, return, (.))

newtype PrincipalFibration a g = PrincipalFibration {twistOnGeom :: GeomSimplex a -> Simplex g}

twistOn :: PrincipalFibration a g -> Simplex a -> Simplex g
twistOn f (NonDegen s) = f `twistOnGeom` s
twistOn f (Degen i s) = degen (f `twistOn` s) i

data TotalSpace a g = TotalSpace a g (PrincipalFibration a g)

newtype TotalSpaceSimplex a = TotalSpaceSimplex a

deriving instance (Eq a) => Eq (TotalSpaceSimplex a)

instance (SSet a, SGrp g) => SSet (TotalSpace a g) where
  type GeomSimplex (TotalSpace a g) = TotalSpaceSimplex (Simplex a, Simplex g)

  isGeomSimplex (TotalSpace a g _) (TotalSpaceSimplex s) = isGeomSimplex (Product a g) s

  geomSimplexDim (TotalSpace a _ _) (TotalSpaceSimplex (s, _)) = simplexDim a s

  geomFace (TotalSpace a g tau) (TotalSpaceSimplex (s, t)) i
    | i == n =
      TotalSpaceSimplex
        <$> prodNormalise
          ( face a s n,
            prod
              (NSimplicesOf (n -1) g)
              (tau `twistOn` s)
              (face g t n)
          )
    | otherwise = TotalSpaceSimplex <$> prodNormalise (face a s i, face g t i)
    where
      n = simplexDim a s

instance (SSet a, SGrp g) => DVF (TotalSpace a g) where
  vf (TotalSpace a g _) (TotalSpaceSimplex s) = coerce $ status (Product a g) s

totalSpaceChainsIso ::
  forall a b.
  CC.Morphism
    (Perturbed (NormalisedChains (Product a b)))
    (NormalisedChains (TotalSpace a b))
totalSpaceChainsIso = CC.Morphism 0 (coerce @((Simplex a, Simplex b) -> _) singleComb)

totalSpaceChainsIsoInv ::
  forall a b.
  CC.Morphism
    (NormalisedChains (TotalSpace a b))
    (Perturbed (NormalisedChains (Product a b)))
totalSpaceChainsIsoInv = CC.Morphism 0 (coerce @((Simplex a, Simplex b) -> _) singleComb)

instance
  ( Effective a,
    Effective g,
    SGrp g
  ) =>
  Effective (TotalSpace a g)
  where
  type Model (TotalSpace a g) = Perturbed (Tensor (Model a) (Model g))

  eff t@(TotalSpace a g tau) =
    composeLeft (NormalisedChains t) (isoToReduction totalSpaceChainsIso totalSpaceChainsIsoInv) $
      perturbLeft
        (eff (Product a g))
        (coerce (diff (NormalisedChains (TotalSpace a g tau))) - diff (NormalisedChains (Product a g)))
