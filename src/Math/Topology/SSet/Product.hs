{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

-- | Cartesian product of simplicial sets See as:dvf, as:ez-dvf
--
-- WARNING: You can define a DVF on the product by searching the path
-- (0,0) to (p,q) forwards or backwards. Some resources use forwards,
-- some backwards, we follow Kenzo by going backwards.
module Math.Topology.SSet.Product where

import Control.Category.Constrained (fmap, (.))
import Data.Bits (clearBit, testBit, (.&.))
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
import Math.Topology.SSet.NChains

data Product a b = Product a b

instance (Show a, Show b) => Show (Product a b) where
  show (Product a b) = show a ++ " × " ++ show b

extractCommonDegens :: Word -> Word -> (Word, Word, Word)
extractCommonDegens left right =
  (common, removeDegenMask common left, removeDegenMask common right)
  where
    common = left .&. right

prodNormalise :: (Simplex a, Simplex b) -> Simplex (Product a b)
prodNormalise (FormalDegen leftMask left, FormalDegen rightMask right) =
  let (common, residualLeft, residualRight) = extractCommonDegens leftMask rightMask
   in FormalDegen common (FormalDegen residualLeft left, FormalDegen residualRight right)

prodUnnormalise :: Simplex (Product a b) -> (Simplex a, Simplex b)
prodUnnormalise s = (s >>= fst, s >>= snd) -- nice!

jointlyNonDegen :: (Simplex a, Simplex b) -> Bool
jointlyNonDegen (FormalDegen leftMask _, FormalDegen rightMask _) =
  leftMask .&. rightMask == 0

instance (SSet a, SSet b) => SSet (Product a b) where
  type GeomSimplex (Product a b) = (Simplex a, Simplex b)
  isGeomSimplex (Product a b) (s, t) =
    simplexDim a s == simplexDim b t
      && jointlyNonDegen (s, t)
      && isSimplex a s
      && isSimplex b t

  geomSimplexDim (Product a _) (s, _) = simplexDim a s

  geomFace (Product a b) (s, t) i = prodNormalise (face a s i, face b t i)

  geomNonDegenFaces (Product a b) (s, t)
    | dimension == 0 = []
    | otherwise =
        [ (i, (leftFace, rightFace))
          | i <- [0 .. dimension],
            let leftFace@(FormalDegen leftMask _) = face a s i,
            let rightFace@(FormalDegen rightMask _) = face b t i,
            leftMask .&. rightMask == 0
        ]
    where
      dimension = simplexDim a s

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

prodAssocInv :: Morphism (Product a (Product b c)) (Product (Product a b) c)
prodAssocInv = Morphism $ \(s, tr) ->
  let (t, r) = prodUnnormalise tr
   in prodNormalise (prodNormalise (s, t), r)

prodFunc :: Morphism a a' -> Morphism b b' -> Morphism (Product a b) (Product a' b')
prodFunc m m' = Morphism $ \(s, t) -> prodNormalise (m `onSimplex` s, m' `onSimplex` t)

-- TODO: there is probably some kind of typeclass trickery we could
-- use to implement 'coherence'. It would be nice to specify a
-- type-level mapping (1, (2, 3)) -> ((2, 1), 3) and have it put
-- together the above maps as required.

instance (SSet a, SSet b) => DVF (Product a b) where
  vf = status

data Direction = X | Y | Diag

data PathStep a b
  = PathStep !Direction !Int !(Simplex a) !(Simplex b)
  | PathEnd

-- Walking backwards from (p,q) to (0,0)
pathStep :: Int -> Simplex a -> Simplex b -> PathStep a b
pathStep 0 _ _ = PathEnd
pathStep q s@(FormalDegen sMask sGeom) t@(FormalDegen tMask tGeom)
  | testBit sMask q' = PathStep X q' (FormalDegen (clearBit sMask q') sGeom) t
  | testBit tMask q' = PathStep Y q' s (FormalDegen (clearBit tMask q') tGeom)
  | otherwise = PathStep Diag q' s t
  where
    q' = q - 1
{-# INLINE pathStep #-}

pathUnstep :: Direction -> (Int, Simplex a, Simplex b) -> (Int, Simplex a, Simplex b)
pathUnstep Diag (q, s, t) = (q + 1, s, t)
pathUnstep X (q, s, t) = (q + 1, Degen q s, t)
pathUnstep Y (q, s, t) = (q + 1, s, Degen q t)

incidenceFor :: Int -> Incidence
incidenceFor x = if even x then Pos else Neg

statusStep :: (Int, Simplex a, Simplex b) -> Status (Int, Simplex a, Simplex b)
statusStep (q, s, t) = case pathStep q s t of
  -- Simplex is a target
  PathStep Y q' s' t'
    | PathStep X q'' s'' t'' <- pathStep q' s' t' ->
      Target (pathUnstep Diag (q'', s'', t'')) (incidenceFor (q'' + 1))
  -- Simplex is a source
  PathStep Diag q' s' t' ->
    Source
      (pathUnstep Y $ pathUnstep X (q', s', t'))
      (incidenceFor (q' + 1))
  -- Simplex is critical
  PathEnd -> Critical
  -- Keep searching
  PathStep direction q' s' t' ->
    fmap (pathUnstep direction) (statusStep (q', s', t'))

status :: SSet a => Product a b -> (Simplex a, Simplex b) -> Status (Simplex a, Simplex b)
status (Product a _) (s, t) =
  fmap (\(_, s, t) -> (s, t)) $
    statusStep
      ( simplexDim a s,
        s,
        t
      )

stripProduct :: (Simplex a, Simplex b) -> (GeomSimplex a, GeomSimplex b)
stripProduct (s, t) = (underlyingGeom s, underlyingGeom t)

reconstructProduct :: (SSet a, SSet b) => a -> b -> (GeomSimplex a, GeomSimplex b) -> (Simplex a, Simplex b)
reconstructProduct a b (s, t) =
  let n = geomSimplexDim a s
      m = geomSimplexDim b t
  in (downshiftN n (constantAt s m), constantAt t n)

criticalIso ::
  forall a b.
  CC.Morphism
    (CriticalComplex (NChains (Product a b)))
    (Tensor (NChains a) (NChains b))
criticalIso = fmapBasis $ coerce @((Simplex a, Simplex b) -> _) stripProduct

criticalIsoInv ::
  (SSet a, SSet b) =>
  a ->
  b ->
  CC.Morphism
    (Tensor (NChains a) (NChains b))
    (CriticalComplex (NChains (Product a b)))
criticalIsoInv a b = fmapBasis $ coerce $ reconstructProduct a b

ezReduction ::
  (SSet a, SSet b) =>
  Product a b ->
  Reduction
    (NChains (Product a b))
    (Tensor (NChains a) (NChains b))
ezReduction p@(Product a b) =
  isoToReduction criticalIso (criticalIsoInv a b)
    . dvfReduction (NChains p)

diagMor :: Morphism a (Product a a)
diagMor = Morphism $ \s -> NonDegen (NonDegen s, NonDegen s)

instance (SSet a, Eq (GeomSimplex a)) => Coalgebra (NChains a) where
  counitMor a = CC.Morphism 0 $ \s -> if degree a s == 0 then singleComb () else 0
  delMor (NChains a) = reductionF (ezReduction (Product a a)) . fmap diagMor

instance (Effective a, Effective b) => Effective (Product a b) where
  type Model (Product a b) = Tensor (Model a) (Model b)

  model (Product a b) = Tensor (model a) (model b)

  eff p@(Product a b) =
    tensorEquiv (eff a) (eff b)
      . fromRedLeft
        (NChains (Product a b))
        (Tensor (NChains a) (NChains b))
        (ezReduction p)
