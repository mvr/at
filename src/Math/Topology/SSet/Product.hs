{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

-- | Cartesian product of simplicial sets See as:dvf, as:ez-dvf
--
-- WARNING: You can define a DVF on the product by searching the path
-- (0,0) to (p,q) forwards or backwards. Some resources use forwards,
-- some backwards, we follow Kenzo by going backwards.
module Math.Topology.SSet.Product where

import Control.Category.Constrained (cfmap, return, (.))
import Data.Coerce
import Math.Algebra.ChainComplex hiding (Morphism, FiniteType)
import qualified Math.Algebra.ChainComplex as CC
import Math.Algebra.ChainComplex.Coalgebra
import Math.Algebra.ChainComplex.DVF hiding (DVF)
import Math.Algebra.ChainComplex.Equivalence
import Math.Algebra.ChainComplex.Reduction
import Math.Algebra.ChainComplex.Tensor
import Math.Topology.SSet
import Math.Topology.SSet.DVF
import Math.Topology.SSet.Effective
import Math.Topology.SSet.Morphism
import Math.Topology.SSet.NormalisedChains
import Prelude hiding (id, return, (.))

data Product a b = Product a b

-- NOTE: In bit-field form we can use "Parallel Bits Extract" or
-- similar to do this efficiently. Single x86 instruction!
-- https://stackoverflow.com/questions/21144237/standard-c11-code-equivalent-to-the-pext-haswell-instruction-and-likely-to-be
prodNormalise :: (Simplex a, Simplex b) -> Simplex (Product a b)
prodNormalise (Degen i s, Degen j t)
  | i == j = degen (prodNormalise (s, t)) i
  | i > j = let p = prodNormalise (s, Degen j t)
            in fmap (\(s', t') -> (Degen (i - degenCount p) s', t')) p
  | i < j = let p = prodNormalise (Degen i s, t)
            in fmap (\(s', t') -> (s', Degen (j - degenCount p) t')) p
prodNormalise s = NonDegen s

jointlyNonDegen :: (Simplex a, Simplex b) -> Bool
jointlyNonDegen ss = case prodNormalise ss of
  NonDegen _ -> True
  Degen _ _ -> False

instance (SSet a, SSet b) => SSet (Product a b) where
  type GeomSimplex (Product a b) = (Simplex a, Simplex b)
  isGeomSimplex (Product a b) (s, t) = jointlyNonDegen (s, t) && simplexDim a s == simplexDim b t

  geomSimplexDim (Product a _) (s, _) = simplexDim a s

  geomFace (Product a b) (s, t) i = prodNormalise (face a s i, face b t i)

instance (Pointed a, Pointed b) => Pointed (Product a b) where
  basepoint (Product a b) = (NonDegen $ basepoint a, NonDegen $ basepoint b)

instance (ZeroReduced a, ZeroReduced b) => ZeroReduced (Product a b)

instance (OneReduced a, OneReduced b) => OneReduced (Product a b)

instance (FiniteType a, FiniteType b) => FiniteType (Product a b) where
  geomBasis (Product a b) n = [ (s, t) | s <- allSimplices a n, t <- allSimplices b n, isGeomSimplex (Product a b) (s, t)]

instance (SSet a, SSet b) => DVF (Product a b) where
  vf = status

-- TODO: in bit-field form this could be done by some efficient
-- twiddling
data Direction = X | Y | Diag | End

-- Walking backwards from (p,q) to (0,0)
spathStep :: (Int, Int, Simplex a, Simplex b) -> (Direction, (Int, Int, Simplex a, Simplex b))
spathStep (0, 0, NonDegen s, NonDegen t) = (End, undefined)
spathStep (p, q, Degen i s, t) | i == p = (X, (p - 1, q, s, t))
spathStep (p, q, s, Degen j t) | j == q = (Y, (p, q - 1, s, t))
spathStep (p, q, s, t) = (Diag, (p - 1, q - 1, s, t))

spathUnstep :: Direction -> (Int, Int, Simplex a, Simplex b) -> (Int, Int, Simplex a, Simplex b)
spathUnstep Diag (p, q, s, t) = (p + 1, q + 1, s, t)
spathUnstep X (p, q, s, t) = (p + 1, q, Degen (p + 1) s, t)
spathUnstep Y (p, q, s, t) = (p, q + 1, s, Degen (p + 1) t)
spathUnstep End (p, q, s, t) = undefined

incidenceFor :: Int -> Incidence
incidenceFor x = if even x then Pos else Neg

-- Little worried about signs in here, likely off by 1
statusStep :: (SSet a, SSet b) => Product a b -> (Int, Int, Simplex a, Simplex b) -> Status (Int, Int, Simplex a, Simplex b)
statusStep prd pqst = case spathStep pqst of
  -- Simplex is a source
  (X, pqst')
    | (Y, (p'', q'', s'', t'')) <- spathStep pqst' ->
      Target (spathUnstep Diag (p'', q'', s'', t'')) (incidenceFor $ geomSimplexDim prd (s'', t''))
  -- Simplex is a target
  (Diag, (p', q', s', t')) -> Source (spathUnstep X $ spathUnstep Y (p', q', s', t')) (incidenceFor $ geomSimplexDim prd (s', t'))
  -- Simplex is critical
  (End, _) -> Critical
  -- Keep searching
  (d, pqst') -> fmap (spathUnstep d) (statusStep prd pqst')

status :: (SSet a, SSet b) => Product a b -> (Simplex a, Simplex b) -> Status (Simplex a, Simplex b)
status (Product a b) (s, t) =
  fmap (\(_, _, s, t) -> (s, t)) $
    statusStep
      (Product a b)
      ( simplexDim a s - degenCount s,
        simplexDim b t - degenCount t,
        s,
        t
      )

criticalIso ::
  forall a b.
  CC.Morphism
    (CriticalComplex (NormalisedChains (Product a b)))
    (Tensor (NormalisedChains a) (NormalisedChains b))
criticalIso = fmapBasis $ coerce @((Simplex a, Simplex b) -> _) $ \(s, t) -> (underlyingGeom s, underlyingGeom t)

criticalIsoInv ::
  (SSet a, SSet b) =>
  a ->
  b ->
  CC.Morphism
    (Tensor (NormalisedChains a) (NormalisedChains b))
    (CriticalComplex (NormalisedChains (Product a b)))
criticalIsoInv a b = fmapBasis $
  coerce $
    \(s, t) ->
      let n = geomSimplexDim a s
          m = geomSimplexDim b t
       in (downshiftN n (constantAt s m), constantAt t n)

ezReduction ::
  (SSet a, SSet b) =>
  Product a b ->
  Reduction
    (NormalisedChains (Product a b))
    (Tensor (NormalisedChains a) (NormalisedChains b))
ezReduction p@(Product a b) =
  isoToReduction criticalIso (criticalIsoInv a b)
    . dvfReduction (NormalisedChains p)

diagMor :: Morphism a (Product a a)
diagMor = Morphism $ \s -> NonDegen (NonDegen s, NonDegen s)

instance (SSet a, Eq (GeomSimplex a)) => Coalgebra (NormalisedChains a) where
  counitMor a = CC.Morphism 0 $ \s -> if degree a s == 0 then return () else 0
  delMor (NormalisedChains a) = reductionF (ezReduction (Product a a)) . cfmap diagMor

instance (Effective a, Effective b) => Effective (Product a b) where
  type Model (Product a b) = Tensor (Model a) (Model b)

  eff p@(Product a b) =
    tensorEquiv (eff a) (eff b)
      . fromRedLeft
        (NormalisedChains (Product a b))
        (Tensor (NormalisedChains a) (NormalisedChains b))
        (ezReduction p)
