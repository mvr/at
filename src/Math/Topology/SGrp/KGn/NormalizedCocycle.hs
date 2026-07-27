-- | Degreewise coordinates for normalized cocycles on standard simplices,
-- together with their change of coordinates to and from the standard
-- inverse Dold--Kan surjection summands.
module Math.Topology.SGrp.KGn.NormalizedCocycle (
  CocycleCoordinate,
  CocycleValues (..),
  cocycleCoordinateVertices,
  cocycleValuesToDoldKan,
  doldKanToCocycleValues,
)
where

import Data.List (unsnoc)
import Data.Maybe (fromMaybe, mapMaybe)

import Math.Algebra.Group
import Math.Topology.SGrp.KGn.DoldKan

-- | An independent normalized @n@-face of a @q@-simplex. A coordinate
-- @c = [c_0,...,c_(n-1)]@ denotes the face with vertices
-- @[c_0,...,c_(n-1),c_(n-1)+1]@. The empty coordinate denotes vertex zero.
type CocycleCoordinate = [Int]

-- | Independent normalized-face values in one simplicial degree.
data CocycleValues e = CocycleValues
  { cocycleValuesSimplexDegree :: !Int,
    cocycleValuesCoordinates :: [(CocycleCoordinate, e)]
  }
  deriving (Eq, Ord, Show, Functor, Foldable, Traversable)

-- | The vertices of the independent face represented by a coordinate.
cocycleCoordinateVertices :: CocycleCoordinate -> [Int]
cocycleCoordinateVertices coordinate = case unsnoc coordinate of
  Nothing -> [0]
  Just (initial, final) -> initial ++ [final, final + 1]

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
  CocycleValues (Element c) ->
  DoldKanSimplex (Element c)
cocycleValuesToDoldKan (DoldKanKGn n c) (CocycleValues q values) =
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
  CocycleValues (Element c)
doldKanToCocycleValues (DoldKanKGn n c) (DoldKanSimplex q s) =
  CocycleValues q (mapMaybe nonzeroValue coordinates)
  where
    coordinates =
      surjectionTransitions <$> doldKanSurjections n q
    nonzeroValue coordinate
      | value == unit c = Nothing
      | otherwise = Just (coordinate, value)
      where
        value = valueAt coordinate
    valueAt coordinate = sectionSum c coordinate s
