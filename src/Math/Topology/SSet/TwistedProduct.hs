{-# LANGUAGE InstanceSigs #-}
-- | A principal \(G\)-bundle over \(A\), represented as a degree (-1) map of
-- simplicial sets \(τ : A \to G\).
--
-- A twisting operation can be turned into a twisting cochain of the
-- corresponding normalised chain complexes, but this process is not
-- so simple. There are a few ways of obtaining it: a direct
-- complicated method by Szczarba, a recursive definition by Morace
-- and Prouté, and what Kenzo does. That is:
--
-- 1. Start with the Eilenberg-Zilber reduction \(G × A ⇛ G ⊗ A \)
-- 2. Determine the perturbation on \(G × A\) that yields \(G ×_τ A\)
-- 3. Use the perturbation lemma to transfer this down to \(G ⊗_t A\)
-- 4. Extract the twisting cochain \(t\) from \(G ⊗_t A\)
module Math.Topology.SSet.TwistedProduct where

-- The following right-action equations are satisfied:
-- \[
-- \begin{aligned}
--   ∂_0(τ b) &= τ(∂_1 b) · [τ(∂_0 b)]^{-1} \\
--   ∂_i(τ b) &= τ(∂_{i+1} b) && \text{if } i > 0 \\
--   η_i(τ b) &= τ(η_{i+1} b) \\
--   e_n &= τ(η_0 b)
-- \end{aligned}
-- \]
--

import Math.Algebra.ChainComplex hiding (FiniteType, Morphism)
import qualified Math.Algebra.ChainComplex as CC
import Math.Algebra.ChainComplex.DVF (FiniteCritical)
import Math.Algebra.ChainComplex.Equivalence
import Math.Algebra.ChainComplex.Reduction
import Math.Algebra.ChainComplex.Tensor
import Math.Topology.SGrp
import Math.Topology.SSet
import Math.Topology.SSet.DVF
import Math.Topology.SSet.Effective
import Math.Topology.SSet.NChains
import Math.Topology.SSet.Product
import Prelude hiding (id, return, (.))

type RightAction g f = Morphism (Product f g) f

newtype Twist b g = Twist {twistOnGeom :: GeomSimplex b -> Simplex g}

twistOnFor :: (SSet b, Pointed g) => b -> g -> Twist b g -> Simplex b -> Simplex g
twistOnFor a g f (NonDegen s) = f `twistOnGeom` s
twistOnFor a g f (Degen i s)
  | i == 0 = constantAt (geomBasepoint g) (simplexDim a s)
  | otherwise = degen (twistOnFor a g f s) (i - 1)

pullback :: (SSet b, Pointed g) => b -> g -> Twist b g -> Morphism a b -> Twist a g
pullback b g t f = Twist $ \a -> twistOnFor b g t (f `onGeomSimplex` a)

data TwistedProduct f b g = TwistedProduct f b g (RightAction g f) (Twist b g)

type TotalSpace b g = TwistedProduct g b g

totalSpace :: (SGrp g) => b -> g -> Twist b g -> TotalSpace b g
totalSpace b g tau = TwistedProduct g b g (prodMor g) tau

instance (SSet f, SSet b, SGrp g) => SSet (TwistedProduct f b g) where
  type GeomSimplex (TwistedProduct f b g) = (Simplex f, Simplex b)

  isGeomSimplex (TwistedProduct f b _ _ _) = isGeomSimplex (Product f b)

  geomSimplexDim (TwistedProduct _ b _ _ _) (_, base) = simplexDim b base

  geomFace (TwistedProduct f b g act tau) (fibre, base) i
    | i == 0 =
        prodNormalise
          ( act `onSimplex` prodNormalise (face f fibre 0, twistOnFor b g tau base),
            face b base 0
          )
    | otherwise = prodNormalise (face f fibre i, face b base i)

instance (FiniteType f, FiniteType b, SGrp g) => FiniteType (TwistedProduct f b g) where
  geomBasis (TwistedProduct f b _ _ _) n = [(fibre, base) | fibre <- allSimplices f n, base <- allSimplices b n, isGeomSimplex (Product f b) (fibre, base)]

instance (SSet f, SSet b, SGrp g) => DVF (TwistedProduct f b g) where
  vf (TwistedProduct f b _ _ _) = status (Product f b)

instance
  (FiniteType f, FiniteType b, SGrp g) =>
  FiniteCritical (NChains (TwistedProduct f b g))

totalSpaceChainsIso ::
  CC.Morphism
    (Perturbed (NChains (Product f b)))
    (NChains (TwistedProduct f b g))
totalSpaceChainsIso = basisMorphism (\simplex -> simplex)

totalSpaceChainsIsoInv ::
  CC.Morphism
    (NChains (TwistedProduct f b g))
    (Perturbed (NChains (Product f b)))
totalSpaceChainsIsoInv = basisMorphism (\simplex -> simplex)

-- | The twisting changes only the zeroth face of a product simplex.
twistedProductPerturbation ::
  (SSet f, SSet b, SGrp g) =>
  TwistedProduct f b g ->
  CC.Morphism (NChains (Product f b)) (NChains (Product f b))
twistedProductPerturbation t@(TwistedProduct f b _ _ _) =
  CC.Morphism (-1) perturb
  where
    perturb simplex@(_, base)
      | simplexDim b base == 0 = 0
      | otherwise = asChain twistedFace - asChain untwistedFace
      where
        twistedFace = geomFace t simplex 0
        untwistedFace = geomFace (Product f b) simplex 0

instance
  ( Effective f,
    Effective b,
    SGrp g
  ) =>
  Effective (TwistedProduct f b g)
  where
  type Model (TwistedProduct f b g) = Perturbed (Tensor (Model f) (Model b))

  eff t@(TwistedProduct f b _ _ _) =
    composeLeft (NChains t) (isoToReduction totalSpaceChainsIso totalSpaceChainsIsoInv) $
      perturbLeft
        (eff (Product f b))
        (twistedProductPerturbation t)
