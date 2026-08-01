-- | Normalized cocycle coordinates and their Dold--Kan classifying maps.
module Math.Topology.SGrp.KGn.DoldKan.Cocycle (
  CocycleCoordinate,
  CocycleFaceValues (..),
  cocycleCoordinateVertices,
  evaluateCocycleFaces,
  cocycleValuesToDoldKan,
  doldKanToCocycleValues,
  cocycleDoldKanMap,
  cocycleClassifyingMap,
)
where

import Control.Category.Constrained ((.))
import Data.List (unsnoc)
import Data.Maybe (fromMaybe, mapMaybe)
import Prelude hiding ((.))

import qualified Math.Algebra.ChainComplex as CC
import Math.Algebra.ChainComplex.Equivalence (equivalenceForward)
import Math.Algebra.Group
import Math.Topology.SGrp.KGn.DoldKan
import qualified Math.Topology.SGrp.KGn.DoldKan as DoldKan
import Math.Topology.SGrp.KGn.DoldKan.Wbar (
  DoldKanWbarModel (CoefficientGroup, emCoefficientGroup, emDegree),
  doldKanComparison,
 )
import Math.Topology.SGrp.Wbar (Wbar (Wbar))
import Math.Topology.SSet
import Math.Topology.SSet.Effective
import Math.Topology.SSet.NChains

-- | An independent normalized @n@-face of a @q@-simplex. A coordinate
-- @c = [c_0,...,c_(n-1)]@ denotes the face with vertices
-- @[c_0,...,c_(n-1),c_(n-1)+1]@. The empty coordinate denotes vertex zero.
type CocycleCoordinate = [Int]

-- | Independent normalized-face values in one simplicial degree.
data CocycleFaceValues e = CocycleFaceValues
  { cocycleSimplexDegree :: !Int,
    cocycleFaceValues :: [(CocycleCoordinate, e)]
  }
  deriving (Eq, Ord, Show, Functor, Foldable, Traversable)

-- | The vertices of the independent face represented by a coordinate.
cocycleCoordinateVertices :: CocycleCoordinate -> [Int]
cocycleCoordinateVertices coordinate = case unsnoc coordinate of
  Nothing -> [0]
  Just (initial, final) -> initial ++ [final, final + 1]

cocycleCoordinateFaces ::
  SSet a =>
  a ->
  Int ->
  Int ->
  GeomSimplex a ->
  [(CocycleCoordinate, Simplex a)]
cocycleCoordinateFaces a n q simplex =
  coordinateFace <$> doldKanSurjections n q
  where
    coordinateFace (DoldKanSurjection _ coordinate) =
      ( coordinate,
        applyFaceOperator
          a
          (FaceOperator q (cocycleCoordinateVertices coordinate))
          (NonDegen simplex)
      )

-- | Evaluate a cocycle on the independent normalized faces of each source
-- simplex.
evaluateCocycleFaces ::
  ( SSet a,
    Group c,
    Eq (Element c)
  ) =>
  a ->
  c ->
  CC.Cocycle (NChains a) c ->
  GeomSimplex a ->
  CocycleFaceValues (Element c)
evaluateCocycleFaces a c cocycle@(CC.Cocycle (CC.Cochain n _)) simplex =
  CocycleFaceValues q $
    mapMaybe evaluateFace (cocycleCoordinateFaces a n q simplex)
  where
    q = geomSimplexDim a simplex
    evaluateFace (_, Degen _ _) = Nothing
    evaluateFace (coordinate, NonDegen faceSimplex) =
      let value = CC.cocycleOnBasis cocycle faceSimplex
       in if value == unit c
            then Nothing
            else Just (coordinate, value)

isSection :: CocycleCoordinate -> DoldKanSurjection -> Bool
isSection coordinate (DoldKanSurjection _ transitions) =
  go coordinate transitions
  where
    go [] [] = True
    go [vertex] [transition] = vertex == transition
    go (lower : vertices@(upper : _)) (transition : rest) =
      lower <= transition
        && transition < upper
        && go vertices rest
    go _ _ = False

sectionSum ::
  Abelian c =>
  c ->
  CocycleCoordinate ->
  [(DoldKanSurjection, Element c)] ->
  Element c
sectionSum c coordinate = foldl' add (unit c)
  where
    add total (surjection, value)
      | isSection coordinate surjection = prod c total value
      | otherwise = total

-- | Change from independent normalized-face values to the standard
-- surjection summands. If @y_beta@ is the value on a face @beta@, then
--
-- @
-- y_beta = sum { x_alpha | alpha . beta = id }.
-- @
--
-- The inverse system is triangular in descending lexicographic order.
cocycleValuesToDoldKan ::
  (Abelian c, Eq (Element c)) =>
  DoldKanKGn c ->
  CocycleFaceValues (Element c) ->
  DoldKanSimplex (Element c)
cocycleValuesToDoldKan (DoldKanKGn n c) (CocycleFaceValues q values) =
  DoldKanSimplex q $ foldl' solve [] (reverse $ doldKanSurjections n q)
  where
    valueAt coordinate = fromMaybe (unit c) (lookup coordinate values)
    solve solved surj@(DoldKanSurjection _ coordinate) =
      let value =
            prod
              c
              (valueAt coordinate)
              (inv c (sectionSum c coordinate solved))
       in if value == unit c
            then solved
            else (surj, value) : solved

-- | Change from the standard surjection summands to independent
-- normalized-face values by evaluating the section pairing.
doldKanToCocycleValues ::
  (Abelian c, Eq (Element c)) =>
  DoldKanKGn c ->
  DoldKanSimplex (Element c) ->
  CocycleFaceValues (Element c)
doldKanToCocycleValues (DoldKanKGn n c) (DoldKanSimplex q s) =
  CocycleFaceValues q $
    mapMaybe nonzeroValue (doldKanSurjections n q)
  where
    nonzeroValue (DoldKanSurjection _ coordinate) =
      let value = sectionSum c coordinate s
       in if value == unit c
            then Nothing
            else Just (coordinate, value)

-- | The inverse Dold--Kan image of a cocycle.
cocycleDoldKanMap ::
  ( Effective a,
    Abelian c,
    Eq (Element c)
  ) =>
  a ->
  c ->
  CC.Cocycle (Model a) c ->
  Morphism a (DoldKanKGn c)
cocycleDoldKanMap a c cocycle = Morphism $ \simplex ->
  DoldKan.normalise $
    cocycleValuesToDoldKan
      (DoldKanKGn n c)
      (evaluateCocycleFaces a c sourceCocycle simplex)
  where
    sourceCocycle =
      CC.pullbackCocycle c (equivalenceForward (eff a)) cocycle
    n = CC.cocycleDegree sourceCocycle

-- | The classifying map to @K(A,n) = Wbar K(A,n-1)@ represented by a
-- cocycle on the effective model of the source.
cocycleClassifyingMap ::
  (Effective a, DoldKanWbarModel g) =>
  a ->
  g ->
  CC.Cocycle (Model a) (CoefficientGroup g) ->
  Morphism a (Wbar g)
cocycleClassifyingMap a g cocycle
  | CC.cocycleDegree cocycle /= emDegree target =
      error "cocycleClassifyingMap: cocycle and target degrees differ"
  | otherwise =
      doldKanComparison target
        . cocycleDoldKanMap a (emCoefficientGroup target) cocycle
  where
    target = Wbar g
