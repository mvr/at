{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE InstanceSigs #-}

-- | A principle \(G\)-bundle over \(A\), represented as a degree (-1) map of
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

-- TODO: update the following
-- The following equations are satisfied:
-- \[
-- \begin{aligned}
--   ∂_i(τ b) &= τ(∂_i b) && \text{if } i < n - 1 \\
--   ∂_{n−1} (τ b) &= [τ (∂_n b)]^{−1} · τ (∂_{n−1} b) \\
--   η_i(τ_b) &= τ(η_i b) && \text{if } i ≤ n - 1 \\
--   e_n &= τ(η_n b)
-- \end{aligned}
-- \]
--

import Data.Coerce
import Math.Algebra.ChainComplex hiding (Morphism, FiniteType)
import qualified Math.Algebra.ChainComplex as CC
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

type Action g f = Morphism (Product f g) f

newtype Twist b g = Twist { twistOnGeom :: GeomSimplex b -> Simplex g }

twistOnFor :: (SSet b, Pointed g) => b -> g -> Twist b g -> Simplex b -> Simplex g
twistOnFor a g f (NonDegen s) = f `twistOnGeom` s
twistOnFor a g f (Degen i s)
  | i == 0 = constantAt (basepoint g) (simplexDim a s)
  | otherwise = degen (twistOnFor a g f s) (i - 1)

pullback :: (SSet b, Pointed g) => b -> g -> Twist b g -> Morphism a b -> Twist a g
pullback b g t f = Twist $ \a -> twistOnFor b g t (f `onGeomSimplex` a)

data TwistedProduct f b g = TwistedProduct f b g (Action g f) (Twist b g)

type TotalSpace b g = TwistedProduct g b g

totalSpace :: (SGrp g) => b -> g -> Twist b g -> TotalSpace b g
totalSpace b g tau = TwistedProduct g b g (prodMor g) tau

newtype TwistedProductSimplex a g = TwistedProductSimplex a

deriving instance (Eq a) => Eq (TwistedProductSimplex a g)
instance (Show a) => Show (TwistedProductSimplex a g) where
  show (TwistedProductSimplex a) = show a

instance (SSet f, SSet b, SGrp g) => SSet (TwistedProduct f b g) where
  type GeomSimplex (TwistedProduct f b g) = TwistedProductSimplex (Simplex f, Simplex b) g

  isGeomSimplex (TwistedProduct f b _ _ _) (TwistedProductSimplex s) = isGeomSimplex (Product f b) s

  geomSimplexDim (TwistedProduct f _ _ _ _) (TwistedProductSimplex (s, _)) = simplexDim f s

  geomFace (TwistedProduct f b g act tau) (TwistedProductSimplex (s, t)) i
    | i == 0 =
      TwistedProductSimplex
        <$> prodNormalise
          ( act `onSimplex` prodNormalise (face f s 0, twistOnFor b g tau t),
            face b t 0
          )
    | otherwise = TwistedProductSimplex <$> prodNormalise (face f s i, face b t i)

instance (FiniteType b, FiniteType f, SGrp g) => FiniteType (TwistedProduct f b g) where
  geomBasis (TwistedProduct f b _ _ _) n = [ TwistedProductSimplex (s, t) | s <- allSimplices f n, t <- allSimplices b n, isGeomSimplex (Product f b) (s, t)]

instance (SSet f, SSet b, SGrp g) => DVF (TwistedProduct f b g) where
  vf (TwistedProduct f b g _ _) (TwistedProductSimplex s) = coerce $ status (Product f b) s

totalSpaceChainsIso ::
  CC.Morphism
    (Perturbed (NChains (Product f b)))
    (NChains (TwistedProduct f b g))
totalSpaceChainsIso = fmapBasis coerce

totalSpaceChainsIsoInv ::
  CC.Morphism
    (NChains (TwistedProduct f b g))
    (Perturbed (NChains (Product f b)))
totalSpaceChainsIsoInv = fmapBasis coerce

instance
  ( Effective f,
    Effective b,
    SGrp g
  ) =>
  Effective (TwistedProduct f b g)
  where
  type Model (TwistedProduct f b g) = Perturbed (Tensor (Model f) (Model b))

  -- TODO: compute the difference directly, likely much faster
  eff t@(TwistedProduct f b g act tau) =
    composeLeft (NChains t) (isoToReduction totalSpaceChainsIso totalSpaceChainsIsoInv) $
    perturbLeft
        (eff (Product f b))
        (coerce (diff (NChains (TwistedProduct f b g act tau))) - diff (NChains (Product f b)))
