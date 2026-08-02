{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE UndecidableInstances #-}

-- | A principal \(G\)-bundle over \(A\), represented as a degree (-1) map of
-- simplicial sets \(τ : A \to G\).
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
import Math.Algebra.ChainComplex.Equivalence
import Math.Algebra.ChainComplex.Reduction
import Math.Algebra.ChainComplex.Tensor
import Math.Algebra.Combination (singleComb)
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
  | i == 0 = constantAt (basepoint g) (simplexDim a s)
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

  geomSimplexDim (TwistedProduct f _ _ _ _) (s, _) = simplexDim f s

  geomFace (TwistedProduct f b g act tau) (s, t) i
    | i == 0 =
        prodNormalise
          ( act `onSimplex` prodNormalise (face f s 0, twistOnFor b g tau t),
            face b t 0
          )
    | otherwise = prodNormalise (face f s i, face b t i)

instance (FiniteType b, FiniteType f, SGrp g) => FiniteType (TwistedProduct f b g) where
  geomBasis (TwistedProduct f b _ _ _) n = [(s, t) | s <- allSimplices f n, t <- allSimplices b n, isGeomSimplex (Product f b) (s, t)]

instance (SSet f, SSet b, SGrp g) => DVF (TwistedProduct f b g) where
  vf (TwistedProduct f b _ _ _) = status (Product f b)

totalSpaceChainsIso ::
  (SSet f, SSet b, SGrp g) =>
  CC.Morphism
    (Perturbed (NChains (Product f b)))
    (NChains (TwistedProduct f b g))
totalSpaceChainsIso = basisMorphism (\simplex -> simplex)

totalSpaceChainsIsoInv ::
  (SSet f, SSet b, SGrp g) =>
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
    perturb simplex@(s, _)
      | simplexDim f s == 0 = 0
      | otherwise = asChain twistedFace - asChain untwistedFace
      where
        twistedFace = geomFace t simplex 0
        untwistedFace = geomFace (Product f b) simplex 0

    asChain (FormalDegen mask simplex)
      | mask == 0 = singleComb simplex
      | otherwise = 0

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
