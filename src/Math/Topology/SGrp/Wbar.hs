{-# LANGUAGE UndecidableInstances #-}

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

wbarDimension :: WbarSimplex a -> Int
wbarDimension (WbarSimplex unitMask entries) = popCount unitMask + length entries

nullWbar :: WbarSimplex a -> Bool
nullWbar (WbarSimplex unitMask entries) = unitMask == 0 && null entries

wbarSimplex :: Pointed g => g -> [Simplex g] -> WbarSimplex (Simplex g)
wbarSimplex g = go 0 0 []
  where
    go _ unitMask entries [] = WbarSimplex unitMask (reverse entries)
    go position unitMask entries (s : ss)
      | isUnit g s =
          if position >= finiteBitSize unitMask
            then error "wbarSimplex: unit position exceeds mask size"
            else go (position + 1) (setBit unitMask position) entries ss
      | otherwise = go (position + 1) unitMask (s : entries) ss

expandWbarSimplex :: Pointed g => g -> WbarSimplex (Simplex g) -> [Simplex g]
expandWbarSimplex g bar@(WbarSimplex unitMask entries) = go 0 entries
  where
    dimension = wbarDimension bar

    go position remaining
      | position == dimension = []
      | testBit unitMask position =
          constantAt (basepoint g) (dimension - position - 1) : go (position + 1) remaining
    go position (s : ss) = s : go (position + 1) ss
    go _ [] = error "expandWbarSimplex: invalid unit mask"

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
unnormaliseWbar (FormalDegen outerMask core@(WbarSimplex coreUnitMask entries))
  | outerMask == 0 = core
  | otherwise = uncurry WbarSimplex (go outerMask coreUnitMask entries)
  where
    go 0 0 [] = (0, [])
    go outer units remaining
      | testBit outer 0 = prependWbarUnit (go (outer `shiftR` 1) units remaining)
      | testBit units 0 = prependWbarUnit (go (outer `shiftR` 1) (units `shiftR` 1) remaining)
    go outer units (s : ss) =
      prependWbarNonUnit
        (applyDegenMask (outer `shiftR` 1) s)
        (go (outer `shiftR` 1) (units `shiftR` 1) ss)
    go _ _ [] = error "unnormaliseWbar: invalid unit mask"

unnormalise :: Pointed g => g -> Simplex (Wbar g) -> [Simplex g]
unnormalise g simplex = expandWbarSimplex g (unnormaliseWbar simplex)

prependWbarUnit :: (Word, [a]) -> (Word, [a])
prependWbarUnit (unitMask, entries) = (setBit (unitMask `shiftL` 1) 0, entries)

prependWbarNonUnit :: a -> (Word, [a]) -> (Word, [a])
prependWbarNonUnit s (unitMask, entries) = (unitMask `shiftL` 1, s : entries)

prependWbarSimplex :: Pointed g => g -> Simplex g -> (Word, [Simplex g]) -> (Word, [Simplex g])
prependWbarSimplex g s
  | isUnit g s = prependWbarUnit
  | otherwise = prependWbarNonUnit s

consWbar :: Pointed g => g -> Simplex g -> WbarSimplex (Simplex g) -> WbarSimplex (Simplex g)
consWbar g s (WbarSimplex unitMask entries) =
  uncurry WbarSimplex (prependWbarSimplex g s (unitMask, entries))

consNonUnitWbar :: Simplex g -> WbarSimplex (Simplex g) -> WbarSimplex (Simplex g)
consNonUnitWbar s (WbarSimplex unitMask entries) =
  WbarSimplex (unitMask `shiftL` 1) (s : entries)

wbarFaceEntries :: SGrp g => g -> WbarSimplex (Simplex g) -> Int -> WbarSimplex (Simplex g)
wbarFaceEntries g (WbarSimplex unitMask entries) i =
  uncurry WbarSimplex (go unitMask entries i)
  where
    go units remaining 0
      | testBit units 0 = (units `shiftR` 1, remaining)
    go units (_ : rest) 0 = (units `shiftR` 1, rest)
    go units remaining 1
      | testBit units 0 = (units `shiftR` 1, remaining)
    go units (s : rest) 1
      | tailUnits == 0 && null rest = (0, [])
      | testBit tailUnits 0 =
          prependWbarSimplex g (face g s 0) (tailUnits `shiftR` 1, rest)
      | s' : ss <- rest =
          prependWbarSimplex
            g
            (prodMor g `onSimplex` prodNormalise (face g s 0, s'))
            (tailUnits `shiftR` 1, ss)
      | otherwise = error "wbarFaceEntries: invalid unit mask"
      where
        tailUnits = units `shiftR` 1
    go units remaining faceIndex
      | faceIndex > 1 && testBit units 0 =
          prependWbarUnit (go (units `shiftR` 1) remaining (faceIndex - 1))
    go units (s : ss) faceIndex
      | faceIndex > 1 =
          prependWbarSimplex
            g
            (face g s (faceIndex - 1))
            (go (units `shiftR` 1) ss (faceIndex - 1))
    go _ _ _ = error "wbarFaceEntries: invalid face index"

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

  geomFace _ bar _ | nullWbar bar = undefined
  -- TODO: need to make sure this matches with Kenzo's conventions,
  -- multiplying on which side (for abelian groups of course it
  -- doesn't matter)
  geomFace (Wbar g) bar i = normaliseWbar (wbarFaceEntries g bar i)

  geomNonDegenFaces (Wbar g) bar
    | nullWbar bar = []
    | otherwise =
        [ (i, faceBar)
          | i <- [0 .. wbarDimension bar],
            let faceBar = wbarFaceEntries g bar i,
            outerDegenMask faceBar == 0
        ]

instance SGrp g => Pointed (Wbar g) where
  basepoint (Wbar g) = WbarSimplex 0 []

instance (SGrp g) => ZeroReduced (Wbar g)

instance (SGrp g, ZeroReduced g) => OneReduced (Wbar g) -- Not a typo!

instance (SGrp g, ZeroReduced g, FiniteType g) => FiniteType (Wbar g) where
  geomBasis (Wbar g) n =
    filter (isGeomSimplex (Wbar g)) $ fmap (wbarSimplex g) $ sequence $ allSimplices g <$> reverse [0 .. (n - 1)]

prodWbar :: SGrp g => g -> WbarSimplex (Simplex g) -> WbarSimplex (Simplex g) -> WbarSimplex (Simplex g)
prodWbar g (WbarSimplex leftUnits leftEntries) (WbarSimplex rightUnits rightEntries) =
  uncurry WbarSimplex (go leftUnits leftEntries rightUnits rightEntries)
  where
    -- A valid tail with no stored entries consists entirely of units.
    go _ [] rightMask right = (rightMask, right)
    go leftMask left _ [] = (leftMask, left)
    go leftMask left@(s : ss) rightMask right@(t : ts)
      | leftIsUnit && rightIsUnit =
          prependWbarUnit (go nextLeftMask left nextRightMask right)
      | leftIsUnit =
          prependWbarNonUnit t (go nextLeftMask left nextRightMask ts)
      | rightIsUnit =
          prependWbarNonUnit s (go nextLeftMask ss nextRightMask right)
      | otherwise =
          prependWbarSimplex
            g
            (prodMor g `onSimplex` prodNormalise (s, t))
            (go nextLeftMask ss nextRightMask ts)
      where
        leftIsUnit = testBit leftMask 0
        rightIsUnit = testBit rightMask 0
        nextLeftMask = leftMask `shiftR` 1
        nextRightMask = rightMask `shiftR` 1

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
  vf _ (WbarSimplex 0 []) = Critical
  -- A positive-dimensional geometric simplex cannot start with a unit:
  -- a leading unit is precisely its zeroth outer degeneracy.
  vf (Wbar g) (WbarSimplex unitMask (s : ss))
    | testBit unitMask 0 = error "Wbar.vf: invalid leading unit"
    | nss <- normaliseWbar (WbarSimplex (unitMask `shiftR` 1) ss) =
      case vf (Product (Wbar g) g) (nss, s) of
        Source (ts', t') i -> Source (consWbar g t' (unnormaliseWbar ts')) (flipIncidence i)
        Target (ss', s') i -> Target (consWbar g s' (unnormaliseWbar ss')) (flipIncidence i)
        Critical -> case vf (Wbar g) (underlyingGeom nss) of
          Source nss' i ->
            Source
              (consNonUnitWbar (degen s 0) (unnormaliseWbar (downshift (fmap (const nss') nss))))
              (flipIncidence i)
          Target ntt' i ->
            Target
              (consNonUnitWbar (upshift s) (unnormaliseWbar (upshift (fmap (const ntt') nss))))
              (flipIncidence i)
          Critical -> Critical
  vf _ _ = error "Wbar.vf: invalid unit mask"

stripBar :: Pointed g => g -> GeomSimplex (Wbar g) -> [GeomSimplex g]
stripBar _ (WbarSimplex _ entries) = fmap underlyingGeom entries

reconstructBar :: SGrp g => g -> [GeomSimplex g] -> GeomSimplex (Wbar g)
reconstructBar _ [] = WbarSimplex 0 []
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
canonicalTwist g = Twist $ \(WbarSimplex _ entries) -> case entries of
  [] -> basepointSimplex g
  s : _ -> s
