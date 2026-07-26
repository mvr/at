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
module Math.Topology.SGrp.KGn.DoldKan
  ( DoldKanKGn (..),
    DoldKanSurjection (..),
    DoldKanSimplex (..),
    DoldKanGeomSimplex (..),
    doldKanSimplex,
    doldKanSurjections,
    doldKanSurjectionValue,
    doldKanSurjectionValues,
    normalise,
    unnormalise,
  )
where

import Data.Bifunctor (first)
import Data.Maybe (mapMaybe)

import Math.Algebra.Group
import qualified Math.Topology.SGrp as SGrp
import Math.Topology.SSet
import Math.Topology.SSet.NSimplex (choose)

-- | The inverse Dold--Kan construction on @A[n]@.
data DoldKanKGn c = DoldKanKGn
  { doldKanDegree :: !Int,
    doldKanCoefficientGroup :: c
  }

-- | A monotone surjection @[q] ->> [n]@, encoded by the @n@ positions at
-- which its value increases. Thus @[1,3]@ in simplicial degree four denotes
-- the sequence @[0,0,1,1,2]@.
data DoldKanSurjection = DoldKanSurjection
  { surjectionSimplexDegree :: !Int,
    surjectionTransitions :: [Int]
  }
  deriving (Eq, Ord)

instance Show DoldKanSurjection where
  show = show . surjectionTransitions

-- | The monotone surjections @[q] ->> [n]@, represented by their transition
-- positions.
doldKanSurjections :: Int -> Int -> [DoldKanSurjection]
doldKanSurjections n q
  | n < 0 || q < n = []
  | otherwise = DoldKanSurjection q <$> choose n [0 .. q - 1]

strictlyIncreasing :: Ord a => [a] -> Bool
strictlyIncreasing xs = and (zipWith (<) xs (drop 1 xs))

-- | Evaluate an encoded surjection at one source vertex.
doldKanSurjectionValue :: DoldKanSurjection -> Int -> Int
doldKanSurjectionValue surjection vertex =
  length (takeWhile (< vertex) (surjectionTransitions surjection))

-- | Evaluate an encoded surjection on all of its source vertices.
doldKanSurjectionValues :: DoldKanSurjection -> [Int]
doldKanSurjectionValues (DoldKanSurjection q transitions) =
  concat (zipWith replicate runLengths [0 ..])
  where
    runLengths = zipWith (-) (transitions ++ [q]) (-1 : transitions)

isDoldKanSurjection :: DoldKanKGn c -> Int -> DoldKanSurjection -> Bool
isDoldKanSurjection target q surjection =
  surjectionSimplexDegree surjection == q
    && length transitions == doldKanDegree target
    && strictlyIncreasing transitions
    && all (\i -> i >= 0 && i < q) transitions
  where
    transitions = surjectionTransitions surjection

-- | A vector in the standard surjection summands of one Dold--Kan simplex.
data DoldKanSimplex e = DoldKanSimplex
  { simplexDegree :: !Int,
    simplexSummands :: [(DoldKanSurjection, e)]
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
isCanonicalDoldKanSimplex target (DoldKanSimplex q summands) =
  q >= 0
    && all (isDoldKanSurjection target q . fst) summands
    && strictlyIncreasing (fst <$> summands)
    && all ((/= unit c) . snd) summands
  where
    c = doldKanCoefficientGroup target

-- | Construct a canonical sparse vector in one simplicial degree.
doldKanSimplex ::
  (Abelian c, Eq (Element c)) =>
  DoldKanKGn c ->
  Int ->
  [(DoldKanSurjection, Element c)] ->
  DoldKanSimplex (Element c)
doldKanSimplex target q summands
  | q < 0 = error "doldKanSimplex: negative simplex degree"
  | not (all (isDoldKanSurjection target q . fst) summands) =
      error "doldKanSimplex: incompatible surjection"
  | otherwise =
      DoldKanSimplex q $
        normaliseGroupTerms (doldKanCoefficientGroup target) summands

-- An element is in the image of s_i exactly when every surjection occurring
-- with nonzero coefficient is constant on the edge [i,i+1].
commonRepeatPositions :: DoldKanSimplex e -> [Int]
commonRepeatPositions (DoldKanSimplex q summands) =
  filter isCommonRepeat [0 .. q - 1]
  where
    isCommonRepeat i =
      all ((i `notElem`) . surjectionTransitions . fst) summands

reindexSurjection :: Int -> (Int -> Int) -> DoldKanSurjection -> DoldKanSurjection
reindexSurjection q reindex (DoldKanSurjection _ transitions) =
  DoldKanSurjection q (reindex <$> transitions)

factorRepeats :: DoldKanSimplex e -> [Int] -> DoldKanSimplex e
factorRepeats (DoldKanSimplex q summands) repeats =
  DoldKanSimplex coreDegree $
    first (reindexSurjection coreDegree reindex) <$> summands
  where
    coreDegree = q - length repeats
    reindex i = i - length (takeWhile (< i) repeats)

-- | Express every common collapsed edge as a formal simplicial degeneracy.
normalise :: DoldKanSimplex e -> FormalDegen (DoldKanGeomSimplex e)
normalise simplex =
  foldl' degen (NonDegen (DoldKanGeomSimplex core)) repeats
  where
    repeats = commonRepeatPositions simplex
    core = factorRepeats simplex repeats

-- | Expand formal degeneracies back into the standard surjection summands.
unnormalise :: FormalDegen (DoldKanGeomSimplex e) -> DoldKanSimplex e
unnormalise simplex =
  DoldKanSimplex expandedDegree $
    first (reindexSurjection expandedDegree liftTransition)
      <$> simplexSummands core
  where
    core = underlyingDoldKanSimplex (underlyingGeom simplex)
    expandedDegree = simplexDegree core + degenCount simplex
    repeats = reverse (degenList simplex)
    liftTransition i = foldl' skipRepeat i repeats
    skipRepeat i repeat
      | repeat <= i = i + 1
      | otherwise = i

composeFace :: Int -> DoldKanSurjection -> Maybe DoldKanSurjection
composeFace i (DoldKanSurjection q transitions)
  | singletonFibre = Nothing
  | otherwise =
      Just (DoldKanSurjection (q - 1) (shiftTransition <$> transitions))
  where
    singletonFibre =
      (i == 0 || i - 1 `elem` transitions)
        && (i == q || i `elem` transitions)
    shiftTransition transition
      | transition < i = transition
      | otherwise = transition - 1

instance (Abelian c, Ord (Element c)) => SSet (DoldKanKGn c) where
  type GeomSimplex (DoldKanKGn c) = DoldKanGeomSimplex (Element c)

  isGeomSimplex target (DoldKanGeomSimplex simplex) =
    isCanonicalDoldKanSimplex target simplex
      && null (commonRepeatPositions simplex)

  geomSimplexDim _ = simplexDegree . underlyingDoldKanSimplex

  geomFace target (DoldKanGeomSimplex (DoldKanSimplex q summands)) i
    | q <= 0 = error "DoldKan.geomFace: face of a vertex"
    | i < 0 || i > q = error "DoldKan.geomFace: invalid face index"
    | otherwise =
        normalise $
          doldKanSimplex target (q - 1) (mapMaybe composeSummand summands)
    where
      composeSummand (surjection, value) =
        (,value) <$> composeFace i surjection

instance (Abelian c, Ord (Element c)) => Pointed (DoldKanKGn c) where
  basepoint target = DoldKanGeomSimplex (doldKanSimplex target 0 [])

instance
  (Abelian c, FiniteGroup c, Ord (Element c)) =>
  FiniteType (DoldKanKGn c)
  where
  geomBasis target q
    | q < 0 = []
    | otherwise =
        filter (isGeomSimplex target) $
          fromValues
            <$> sequence (replicate (length surjections) (elements c))
    where
      c = doldKanCoefficientGroup target
      surjections = doldKanSurjections (doldKanDegree target) q
      fromValues values =
        DoldKanGeomSimplex $
          doldKanSimplex target q (filter ((/= unit c) . snd) (zip surjections values))

instance (Abelian c, Ord (Element c)) => SGrp.SGrp (DoldKanKGn c) where
  prodMor target = Morphism $ \(left, right) ->
    let DoldKanSimplex q leftSummands = unnormalise left
        DoldKanSimplex _ rightSummands = unnormalise right
     in normalise $
          doldKanSimplex target q (leftSummands ++ rightSummands)

  invMor target =
    Morphism $
      NonDegen . fmap (inv (doldKanCoefficientGroup target))

instance (Abelian c, Ord (Element c)) => SGrp.SAb (DoldKanKGn c)
