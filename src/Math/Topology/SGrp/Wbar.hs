{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ViewPatterns #-}

-- | Classifying spaces for simplicial groups
-- Wbar : sGrp -> 0-reduced sSet_*
-- See <https://ncatlab.org/nlab/show/simplicial+classifying+space>
-- In the Kenzo source, this is spread over
-- classifying-spaces.lisp, classifying-spaces-dvf.lisp, cl-space-efhm.lisp
-- Also anromero/resolutions.lisp in the fork
module Math.Topology.SGrp.Wbar where

import Control.Category.Constrained ((.))
import Data.Bits
import Data.Coerce
import Prelude hiding (id, return, (.))

import qualified Math.Algebra.Bicomplex as Bi
import qualified Math.Algebra.ChainComplex as CC (Morphism, UMorphism (..), kozulRule)
import Math.Algebra.Combination (singleComb)
import Math.Algebra.ChainComplex.Algebra.Bar
import Math.Algebra.ChainComplex.DVF hiding (DVF, vf)
import Math.Algebra.ChainComplex.Equivalence
import Math.Algebra.ChainComplex.Reduction
import Math.Topology.SGrp
import Math.Topology.SSet
import Math.Topology.SSet.DVF
import Math.Topology.SSet.Effective
import Math.Topology.SSet.NChains
import Math.Topology.SSet.Product hiding (criticalIso, criticalIsoInv)
import Math.Topology.SSet.TwistedProduct

newtype Wbar g = Wbar g
  deriving (Show)

-- | Bar coordinates with unit entries omitted. Bit @p@ records that the
-- coordinate at position @p@ is the dimension-appropriate unit. Valid values
-- have no unit in the list and no mask bit beyond the represented dimension.
data WbarSimplex a = WbarSimplex {-# UNPACK #-} !Word [a]
  deriving (Show, Eq, Ord)

-- | Logical view of one compressed bar coordinate.
data WbarView a
  = WbarNilView
  | WbarUnitView !(WbarSimplex a)
  | WbarEntryView a !(WbarSimplex a)

viewWbar :: WbarSimplex a -> WbarView a
viewWbar (WbarSimplex 0 []) = WbarNilView
viewWbar (WbarSimplex unitMask entries)
  | testBit unitMask 0 =
      WbarUnitView (WbarSimplex (unitMask `shiftR` 1) entries)
viewWbar (WbarSimplex unitMask (entry : entries)) =
  WbarEntryView entry (WbarSimplex (unitMask `shiftR` 1) entries)
viewWbar _ = error "viewWbar: invalid unit mask"
{-# INLINE viewWbar #-}

-- | The empty bar.
pattern WNil :: WbarSimplex a
pattern WNil = WbarSimplex 0 []

-- | A dimension-appropriate unit followed by the remaining coordinates.
pattern WUnit :: WbarSimplex a -> WbarSimplex a
pattern WUnit rest <- (viewWbar -> WbarUnitView rest)
  where
    WUnit rest =
      case rest of
        WbarSimplex unitMask entries ->
          WbarSimplex (setBit (unitMask `shiftL` 1) 0) entries

-- | A stored nonunit entry followed by the remaining coordinates.
pattern WEntry :: a -> WbarSimplex a -> WbarSimplex a
pattern WEntry entry rest <- (viewWbar -> WbarEntryView entry rest)
  where
    WEntry entry rest =
      case rest of
        WbarSimplex unitMask entries ->
          WbarSimplex (unitMask `shiftL` 1) (entry : entries)

{-# COMPLETE WNil, WUnit, WEntry #-}

wbarDimension :: WbarSimplex a -> Int
wbarDimension (WbarSimplex unitMask entries) = popCount unitMask + length entries

onlyUnitsWbar :: WbarSimplex a -> Bool
onlyUnitsWbar (WbarSimplex _ entries) = null entries

wbarSimplex :: Pointed g => g -> [Simplex g] -> WbarSimplex (Simplex g)
wbarSimplex g = foldr (consWbar g) WNil

expandWbarSimplex :: Pointed g => g -> WbarSimplex (Simplex g) -> [Simplex g]
expandWbarSimplex g bar = go (wbarDimension bar) bar
  where
    go _ WNil = []
    go dimension (WUnit rest) =
      constantAt (basepoint g) (dimension - 1) : go (dimension - 1) rest
    go dimension (WEntry entry rest) =
      entry : go (dimension - 1) rest

normalise :: Pointed g => g -> [Simplex g] -> Simplex (Wbar g)
normalise g ss = normaliseWbar (wbarSimplex g ss)

normaliseWbar :: WbarSimplex (Simplex g) -> Simplex (Wbar g)
normaliseWbar bar
  | outerMask == 0 = NonDegen bar
  | otherwise = FormalDegen outerMask (removeOuterDegens outerMask bar)
  where
    outerMask = outerDegenMask bar

outerDegenMask :: WbarSimplex (Simplex g) -> Word
outerDegenMask (WbarSimplex unitMask entries) = go unitMask maxBound entries
  where
    -- Candidate bit zero and the result are relative to this coordinate.
    go 0 _ [] = 0
    go _ 0 _ = 0
    go units candidates remaining
      | testBit units 0 =
          (candidates .&. 1)
            .|. (go (units `shiftR` 1) (candidates `shiftR` 1) remaining `shiftL` 1)
    go units candidates (FormalDegen mask _ : rest) =
      go (units `shiftR` 1) ((candidates `shiftR` 1) .&. mask) rest `shiftL` 1
    go _ _ [] = error "outerDegenMask: invalid unit mask"

removeOuterDegens :: Word -> WbarSimplex (Simplex g) -> WbarSimplex (Simplex g)
removeOuterDegens 0 bar = bar
removeOuterDegens outerMask (WbarSimplex unitMask entries) =
  WbarSimplex (removeDegenMask outerMask unitMask) (go unitMask outerMask entries)
  where
    go 0 _ [] = []
    go units outer remaining
      | testBit units 0 = go (units `shiftR` 1) (outer `shiftR` 1) remaining
    go units outer (FormalDegen mask s : rest) =
      FormalDegen (removeDegenMask (outer `shiftR` 1) mask) s
        : go (units `shiftR` 1) (outer `shiftR` 1) rest
    go _ _ [] = error "removeOuterDegens: invalid unit mask"

unnormaliseWbar :: Simplex (Wbar g) -> WbarSimplex (Simplex g)
unnormaliseWbar (FormalDegen outerMask core) = go outerMask core
  where
    go 0 bar = bar
    go outer bar
      | testBit outer 0 = WUnit (go (outer `shiftR` 1) bar)
    go outer (WUnit rest) =
      WUnit (go (outer `shiftR` 1) rest)
    go outer (WEntry entry rest) =
      WEntry
        (applyDegenMask (outer `shiftR` 1) entry)
        (go (outer `shiftR` 1) rest)
    go _ WNil = error "unnormaliseWbar: invalid outer degeneracy"

unnormalise :: Pointed g => g -> Simplex (Wbar g) -> [Simplex g]
unnormalise g simplex = expandWbarSimplex g (unnormaliseWbar simplex)

consWbar :: Pointed g => g -> Simplex g -> WbarSimplex (Simplex g) -> WbarSimplex (Simplex g)
consWbar g s
  | isUnit g s = WUnit
  | otherwise = WEntry s

wbarFaceEntries :: SGrp g => g -> WbarSimplex (Simplex g) -> Int -> WbarSimplex (Simplex g)
wbarFaceEntries _ (WUnit rest) 0 = rest
wbarFaceEntries _ (WEntry _ rest) 0 = rest
wbarFaceEntries _ (WUnit rest) 1 = rest
wbarFaceEntries _ (WEntry _ WNil) 1 = WNil
wbarFaceEntries g (WEntry entry (WUnit rest)) 1 =
  consWbar g (face g entry 0) rest
wbarFaceEntries g (WEntry entry (WEntry next rest)) 1 =
  consWbar
    g
    (prodMor g `onSimplex` prodNormalise (face g entry 0, next))
    rest
wbarFaceEntries g (WUnit rest) faceIndex
  | faceIndex > 1 =
      WUnit (wbarFaceEntries g rest (faceIndex - 1))
wbarFaceEntries g (WEntry entry rest) faceIndex
  | faceIndex > 1 =
      consWbar
        g
        (face g entry (faceIndex - 1))
        (wbarFaceEntries g rest (faceIndex - 1))
wbarFaceEntries _ _ _ = error "wbarFaceEntries: invalid face index"

instance (SGrp g) => SSet (Wbar g) where
  -- A non-degenerate simplex is a unit mask and a list of nonunit simplices
  -- of `g`.
  -- (Wbar G)_n = G_n-1 x G_n-2 x ... x G_0
  -- meeting a slightly complicated condition on whether the list
  -- contains a unit and the things preceding that unit are degeneracies
  type GeomSimplex (Wbar g) = WbarSimplex (Simplex g)

  isGeomSimplex (Wbar g) bar@(WbarSimplex unitMask entries) =
    (unitMask == 0 || highestSetBit unitMask < dimension)
      && validEntries 0 entries
      && outerDegenMask bar == 0
    where
      dimension = wbarDimension bar

      validEntries position remaining
        | position == dimension = null remaining
        | testBit unitMask position = validEntries (position + 1) remaining
      validEntries position (s : ss) =
        simplexDim g s == dimension - position - 1
          && not (isUnit g s)
          && isSimplex g s
          && validEntries (position + 1) ss
      validEntries _ [] = False

  geomSimplexDim _ = wbarDimension

  geomFace _ WNil _ = undefined
  -- TODO: need to make sure this matches with Kenzo's conventions,
  -- multiplying on which side (for abelian groups of course it
  -- doesn't matter)
  geomFace (Wbar g) bar i = normaliseWbar (wbarFaceEntries g bar i)

instance SGrp g => Pointed (Wbar g) where
  basepoint _ = WNil

instance (SGrp g) => ZeroReduced (Wbar g)

instance (SGrp g, ZeroReduced g) => OneReduced (Wbar g) -- Not a typo!

instance (SGrp g, ZeroReduced g, FiniteType g) => FiniteType (Wbar g) where
  geomBasis (Wbar g) n =
    filter (isGeomSimplex (Wbar g)) $ fmap (wbarSimplex g) $ sequence $ allSimplices g <$> reverse [0 .. (n - 1)]

prodWbar :: SGrp g => g -> WbarSimplex (Simplex g) -> WbarSimplex (Simplex g) -> WbarSimplex (Simplex g)
prodWbar _ left right | onlyUnitsWbar left = right
prodWbar _ left right | onlyUnitsWbar right = left
prodWbar g (WUnit left) (WUnit right) =
  WUnit (prodWbar g left right)
prodWbar g (WUnit left) (WEntry entry right) =
  WEntry entry (prodWbar g left right)
prodWbar g (WEntry entry left) (WUnit right) =
  WEntry entry (prodWbar g left right)
prodWbar g (WEntry leftEntry left) (WEntry rightEntry right) =
  consWbar
    g
    (prodMor g `onSimplex` prodNormalise (leftEntry, rightEntry))
    (prodWbar g left right)
prodWbar _ _ _ = error "prodWbar: dimension mismatch"

instance (SAb g) => SGrp (Wbar g) where
  prodMor (Wbar g) = Morphism $ \(gs1, gs2) ->
    normaliseWbar (prodWbar g (unnormaliseWbar gs1) (unnormaliseWbar gs2))

  invMor (Wbar g) = Morphism $ \(WbarSimplex unitMask entries) ->
    NonDegen (WbarSimplex unitMask (fmap (invMor g `onSimplex`) entries))

instance (SAb g) => SAb (Wbar g)

-- instance (SGrp g) => Kan (Wbar g)

-- Kenzo implements this via DVF when `g` is a 0-reduced simplicial
-- abelian group. This should be enough to compute homotopy groups of
-- 1-reduced simplicial sets, as the K(G,n)s involved should all be of
-- that type.

-- Other simplicial groups will need the more complicated method
-- described in serre.lisp and cl-space-efhm.lisp

instance (SAb g, ZeroReduced g) => DVF (Wbar g) where
  vf _ WNil = Critical
  -- A positive-dimensional geometric simplex cannot start with a unit:
  -- a leading unit is precisely its zeroth outer degeneracy.
  vf wbar@(Wbar g) (WEntry entry tail) =
    -- Match the tail/head pair first, recursing on the tail only if critical.
    case vf (Product wbar g) (normalisedTail, entry) of
      Source matched incidence ->
        Source (reassemble matched) (flipIncidence incidence)
      Target matched incidence ->
        Target (reassemble matched) (flipIncidence incidence)
      Critical -> tailMatch
    where
      normalisedTail = normaliseWbar tail

      reassemble (tail', entry') =
        consWbar g entry' (unnormaliseWbar tail')

      tailMatch =
        case vf wbar (underlyingGeom normalisedTail) of
          Source tail' incidence ->
            Source
              (reassembleCritical tail')
              (flipIncidence incidence)
          Target tail' incidence ->
            Target
              (reassembleCritical tail')
              (flipIncidence incidence)
          Critical -> Critical

      -- Critical product simplices are canonically reconstructed from their cores.
      reassembleCritical tail' =
        case reconstructProduct wbar g (tail', underlyingGeom entry) of
          (tailSimplex, entrySimplex) ->
            WEntry entrySimplex (unnormaliseWbar tailSimplex)
  vf _ (WUnit _) = error "Wbar.vf: invalid leading unit"

stripBar :: Pointed g => g -> GeomSimplex (Wbar g) -> [GeomSimplex g]
stripBar _ (WbarSimplex _ entries) = fmap underlyingGeom entries

reconstructBar :: SGrp g => g -> [GeomSimplex g] -> GeomSimplex (Wbar g)
reconstructBar _ [] = WNil
reconstructBar g (a:as) = consWbar g a' (unnormaliseWbar b')
  where rest = reconstructBar g as
        (b', a') = reconstructProduct (Wbar g) g (rest, a)

barOrientation :: SSet g => g -> [GeomSimplex g] -> Int
barOrientation g = go . fmap (geomSimplexDim g)
  where
    -- Sum_{i<j} d_i*(d_j+1), from crossing later suspended factors.
    go [] = 0
    go (d:ds) = d * (length ds + sum ds) + go ds

criticalIso ::
  forall g.
  (Pointed g) =>
  g ->
  CC.Morphism
    (CriticalComplex (NChains (Wbar g)))
    (Bar (NChains g))
criticalIso g = CC.Morphism 0 $ coerce @(GeomSimplex (Wbar g) -> _) $ \s ->
  let as = stripBar g s
   in CC.kozulRule (barOrientation g as) (singleComb as)

criticalIsoInv ::
  (SGrp g) =>
  g ->
  CC.Morphism
    (Bar (NChains g))
    (CriticalComplex (NChains (Wbar g)))
criticalIsoInv g = CC.Morphism 0 $ coerce $ \as ->
  CC.kozulRule (barOrientation g as) (singleComb (reconstructBar g as))

wbarReduction ::
  (SAb g, ZeroReduced g) =>
  Wbar g ->
  Reduction
    (NChains (Wbar g))
    (Bar (NChains g))
wbarReduction p@(Wbar g) =
  isoToReduction (criticalIso g) (criticalIsoInv g)
    . dvfReduction (NChains p)

instance (SAb g, Effective g, ZeroReduced g) => Effective (Wbar g) where
  type Model (Wbar g) = Perturbed (TensorSusp (Model g))
  eff (Wbar g) = barEquiv (eff g) . fromRedLeft (NChains (Wbar g)) (Bar (NChains g)) (wbarReduction (Wbar g))

-- | Canonical twisting function \(\bar W G \rightsquigarrow G\),
-- corresponding to the fibre sequence \( G \hookrightarrow W G
-- \twoheadrightarrow \bar W G\). The total space \(W G\) is
-- contractible.

canonicalTwist :: (SGrp g) => g -> Twist (Wbar g) g
canonicalTwist g = Twist $ \bar -> case bar of
  WNil -> basepointSimplex g
  WEntry entry _ -> entry
  WUnit _ -> error "canonicalTwist: invalid leading unit"
