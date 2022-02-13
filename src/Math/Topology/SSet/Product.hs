{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

-- | Cartesian product of simplicial sets See as:dvf, as:ez-dvf
--
-- WARNING: You can define a DVF on the product by searching the path
-- (0,0) to (p,q) forwards or backwards. Some resources use forwards,
-- some backwards, we follow Kenzo by going backwards.
module Math.Topology.SSet.Product where

import Control.Category.Constrained (fmap, (.))
import Data.Coerce
import Prelude hiding (fmap, id, return, (.))

import Math.Algebra.ChainComplex hiding (FiniteType, Morphism)
import qualified Math.Algebra.ChainComplex as CC
import Math.Algebra.ChainComplex.Coalgebra
import Math.Algebra.ChainComplex.DVF hiding (DVF)
import Math.Algebra.ChainComplex.Equivalence
import Math.Algebra.ChainComplex.Reduction
import Math.Algebra.ChainComplex.Tensor
import Math.Algebra.Combination
import Math.Topology.SSet
import Math.Topology.SSet.DVF
import Math.Topology.SSet.Effective
import Math.Topology.SSet.NormalisedChains

data Product a b = Product a b

-- NOTE: In bit-field form we can use "Parallel Bits Extract" or
-- similar to do this efficiently. Single x86 instruction!
-- https://stackoverflow.com/questions/21144237/standard-c11-code-equivalent-to-the-pext-haswell-instruction-and-likely-to-be
prodNormalise :: (Simplex a, Simplex b) -> Simplex (Product a b)
prodNormalise (Degen i s, Degen j t)
  | i == j = degen (prodNormalise (s, t)) i
  | i > j =
    let p = prodNormalise (s, Degen j t)
     in fmap (\(s', t') -> (Degen (i - degenCount p) s', t')) p
  | i < j =
    let p = prodNormalise (Degen i s, t)
     in fmap (\(s', t') -> (s', Degen (j - degenCount p) t')) p
prodNormalise s = NonDegen s

prodUnnormalise :: Simplex (Product a b) -> (Simplex a, Simplex b)
prodUnnormalise s = (s >>= fst, s >>= snd) -- nice!

jointlyNonDegen :: (Simplex a, Simplex b) -> Bool
jointlyNonDegen ss = case prodNormalise ss of
  NonDegen _ -> True
  Degen _ _ -> False

instance (SSet a, SSet b) => SSet (Product a b) where
  type GeomSimplex (Product a b) = (Simplex a, Simplex b)
  isGeomSimplex (Product a b) (s, t) =
    simplexDim a s == simplexDim b t
      && jointlyNonDegen (s, t)
      && isSimplex a s
      && isSimplex b t

  geomSimplexDim (Product a _) (s, _) = simplexDim a s

  geomFace (Product a b) (s, t) i = prodNormalise (face a s i, face b t i)

instance (Pointed a, Pointed b) => Pointed (Product a b) where
  basepoint (Product a b) = (NonDegen $ basepoint a, NonDegen $ basepoint b)

instance (ZeroReduced a, ZeroReduced b) => ZeroReduced (Product a b)

instance (OneReduced a, OneReduced b) => OneReduced (Product a b)

instance (FiniteType a, FiniteType b) => FiniteType (Product a b) where
  geomBasis (Product a b) n = [(s, t) | s <- allSimplices a n, t <- allSimplices b n, isGeomSimplex (Product a b) (s, t)]

prodSym :: Morphism (Product a b) (Product b a)
prodSym = Morphism $ \(s, t) -> NonDegen (t, s)

prodAssoc :: Morphism (Product (Product a b) c) (Product a (Product b c))
prodAssoc = Morphism $ \(st, r) ->
  let (s, t) = prodUnnormalise st
  in prodNormalise (s, prodNormalise (t, r))

prodAssocInv :: Morphism (Product a (Product b c))  (Product (Product a b) c)
prodAssocInv = Morphism $ \(s, tr) ->
  let (t, r) = prodUnnormalise tr
   in prodNormalise (prodNormalise (s, t), r)

prodFunc :: Morphism a a' -> Morphism b b' -> Morphism (Product a b) (Product a' b')
prodFunc m m' = Morphism $ \(s, t) -> prodNormalise (m `onSimplex` s, m' `onSimplex` t)

instance (SSet a, SSet b) => DVF (Product a b) where
  vf = status

-- TODO: in bit-field form this could be done by some efficient
-- twiddling
data Direction = X | Y | Diag | End

-- Walking backwards from (p,q) to (0,0)
spathStep :: (Int, Simplex a, Simplex b) -> (Direction, (Int, Simplex a, Simplex b))
spathStep (0, _, _) = (End, undefined)
spathStep (q, Degen i s, t) | i+1 == q = (X, (q - 1, s, t))
spathStep (q, s, Degen j t) | j+1 == q = (Y, (q - 1, s, t))
spathStep (q, s, t) = (Diag, (q - 1, s, t))

spathUnstep :: Direction -> (Int, Simplex a, Simplex b) -> (Int, Simplex a, Simplex b)
spathUnstep Diag (q, s, t) = (q + 1, s, t)
spathUnstep X (q, s, t) = (q + 1, Degen q s, t)
spathUnstep Y (q, s, t) = (q + 1, s, Degen q t)
spathUnstep End (q, s, t) = undefined

incidenceFor :: Int -> Incidence
incidenceFor x = if even x then Pos else Neg

-- Little worried about signs in here, likely off by 1
statusStep :: (SSet a, SSet b) => Product a b -> (Int, Simplex a, Simplex b) -> Status (Int, Simplex a, Simplex b)
statusStep prd qst = case spathStep qst of
  -- Simplex is a target
  (Y, qst')
    | (X, (q'', s'', t'')) <- spathStep qst' ->
      Target (spathUnstep Diag (q'', s'', t'')) (incidenceFor (q'' + 1))
  -- Simplex is a source
  (Diag, (q', s', t')) ->
    Source
      (spathUnstep Y $ spathUnstep X (q', s', t'))
      (incidenceFor (q' + 1))
  -- Simplex is critical
  (End, _) -> Critical
  -- Keep searching
  (d, qst') -> fmap (spathUnstep d) (statusStep prd qst')

status :: (SSet a, SSet b) => Product a b -> (Simplex a, Simplex b) -> Status (Simplex a, Simplex b)
status (Product a b) (s, t) =
  fmap (\(_, s, t) -> (s, t)) $
    statusStep
      (Product a b)
      ( simplexDim a s,
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
  counitMor a = CC.Morphism 0 $ \s -> if degree a s == 0 then singleComb () else 0
  delMor (NormalisedChains a) = reductionF (ezReduction (Product a a)) . fmap diagMor

instance (Effective a, Effective b) => Effective (Product a b) where
  type Model (Product a b) = Tensor (Model a) (Model b)

  eff p@(Product a b) =
    tensorEquiv (eff a) (eff b)
      . fromRedLeft
        (NormalisedChains (Product a b))
        (Tensor (NormalisedChains a) (NormalisedChains b))
        (ezReduction p)
