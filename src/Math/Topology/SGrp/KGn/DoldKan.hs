{-# LANGUAGE UndecidableInstances #-}

-- | The inverse Dold--Kan construction on a chain complex concentrated in
-- one degree, in its standard surjection-summand presentation:
--
-- @
-- Gamma(A[n])_q = directSum { A | alpha : [q] ->> [n] }.
-- @
--
-- A simplicial operator acts on a summand by composing its indexing
-- surjection. Since @A[n]@ has zero differential, the summand is sent to zero
-- when the composite is not surjective.
module Math.Topology.SGrp.KGn.DoldKan (
  DoldKanKGn (..),
  DoldKanSimplex (..),
  DoldKanGeomSimplex (..),
  normalise,
  unnormalise,
)
where

import Control.Monad (replicateM)
import Data.Bifunctor (first)
import Data.Maybe (mapMaybe)

import Math.Algebra.Group
import qualified Math.Topology.SGrp as SGrp
import Math.Topology.SSet
import Math.Topology.SSet.Surjection

-- | The inverse Dold--Kan construction on @A[n]@.
data DoldKanKGn c = DoldKanKGn
  { doldKanDegree :: !Int,
    doldKanCoefficientGroup :: c
  }

-- | A vector in the standard surjection summands of one Dold--Kan simplex.
data DoldKanSimplex e = DoldKanSimplex
  { simplexDegree :: !Int,
    simplexSummands :: [(Surjection, e)]
  }
  deriving (Eq, Ord, Show, Functor)

-- | A nondegenerate geometric Dold--Kan simplex.
newtype DoldKanGeomSimplex e = DoldKanGeomSimplex
  { underlyingDoldKanSimplex :: DoldKanSimplex e
  }
  deriving (Eq, Ord, Show, Functor)

isCanonicalDoldKanSimplex ::
  (Abelian c, Eq (Element c)) =>
  DoldKanKGn c ->
  DoldKanSimplex (Element c) ->
  Bool
isCanonicalDoldKanSimplex (DoldKanKGn n c) (DoldKanSimplex q as) =
  q >= 0
    && all valid as
    && strictlyIncreasing (fst <$> as)
  where
    valid (a, e) =
      isValidSurjection a
        && surjectionDomainDegree a == q
        && surjectionCodomainDegree a == n
        && e /= unit c

-- Canonicalise coefficients when the surjections are known to be compatible.
normaliseSummands ::
  (Abelian c, Eq (Element c)) =>
  c ->
  Int ->
  [(Surjection, Element c)] ->
  DoldKanSimplex (Element c)
normaliseSummands c q = DoldKanSimplex q . normaliseGroupTerms c

-- An element is in the image of s_i exactly when every surjection occurring
-- with nonzero coefficient is constant on the edge [i,i+1].
commonRepeatPositions :: DoldKanSimplex e -> [Int]
commonRepeatPositions (DoldKanSimplex q as) =
  filter isCommonRepeat [0 .. q - 1]
  where
    isCommonRepeat i = all (repeatsAt i) as
    repeatsAt i (a, _) = surjectionRepeatsAt a i

factorRepeats :: DoldKanSimplex e -> [Int] -> DoldKanSimplex e
factorRepeats (DoldKanSimplex q as) rs =
  DoldKanSimplex (q - length rs) $
    first (removeRepeats rs) <$> as

-- | Express every common collapsed edge as a formal simplicial degeneracy.
normalise :: DoldKanSimplex e -> FormalDegen (DoldKanGeomSimplex e)
normalise s =
  foldl' degen (NonDegen (DoldKanGeomSimplex core)) rs
  where
    rs = commonRepeatPositions s
    core = factorRepeats s rs

-- | Expand formal degeneracies back into the standard surjection summands.
unnormalise :: FormalDegen (DoldKanGeomSimplex e) -> DoldKanSimplex e
unnormalise s =
  DoldKanSimplex (q + length rs) $
    first (insertRepeats rs) <$> as
  where
    DoldKanGeomSimplex (DoldKanSimplex q as) = underlyingGeom s
    rs = reverse (degenList s)

instance (Abelian c, Ord (Element c)) => SSet (DoldKanKGn c) where
  type GeomSimplex (DoldKanKGn c) = DoldKanGeomSimplex (Element c)

  isGeomSimplex k (DoldKanGeomSimplex s) =
    isCanonicalDoldKanSimplex k s
      && null (commonRepeatPositions s)

  geomSimplexDim _ = simplexDegree . underlyingDoldKanSimplex

  geomFace (DoldKanKGn _ c) (DoldKanGeomSimplex (DoldKanSimplex q as)) i
    | q <= 0 = error "DoldKan.geomFace: face of a vertex"
    | i < 0 || i > q = error "DoldKan.geomFace: invalid face index"
    | otherwise =
        normalise $
          normaliseSummands c (q - 1) (mapMaybe (\(a, e) -> (,e) <$> precomposeFace i a) as)

instance (Abelian c, Ord (Element c)) => Pointed (DoldKanKGn c) where
  geomBasepoint _ = DoldKanGeomSimplex (DoldKanSimplex 0 [])

instance
  (Abelian c, FiniteGroup c, Ord (Element c)) =>
  FiniteType (DoldKanKGn c)
  where
  geomBasis (DoldKanKGn n c) q
    | q < 0 = []
    | otherwise = mapMaybe fromValues $ replicateM (length ss) (elements c)
    where
      ss = surjections q n
      fromValues es =
        let s = DoldKanSimplex q $ filter ((/= unit c) . snd) (zip ss es)
         in if null (commonRepeatPositions s)
              then Just (DoldKanGeomSimplex s)
              else Nothing

instance (Abelian c, Ord (Element c)) => SGrp.SGrp (DoldKanKGn c) where
  prodMor (DoldKanKGn _ c) = Morphism $ \(s, t) ->
    let DoldKanSimplex q as = unnormalise s
        DoldKanSimplex _ bs = unnormalise t
     in normalise $
          normaliseSummands c q (as ++ bs)

  invMor (DoldKanKGn _ c) =
    Morphism $
      NonDegen . fmap (inv c)

instance (Abelian c, Ord (Element c)) => SGrp.SAb (DoldKanKGn c)
